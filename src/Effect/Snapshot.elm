module Effect.Snapshot exposing (PercyApiKey(..), PublicFiles, Snapshot, uploadSnapshots)

import Base64
import Bytes exposing (Bytes)
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Nonempty exposing (Nonempty(..))
import SHA256
import Task exposing (Task)
import Test.Html.Internal.Inert
import Test.Html.Query.Internal
import Url
import Url.Builder


{-| Name of the snapshot and the html in it to be diffed.
-}
type alias Snapshot msg =
    { name : String, html : Html msg }


{-| Files in your public folder such as `images/profile-image.png` or `favicon.ico`.
-}
type alias PublicFiles =
    { filepath : String
    , content : Bytes
    }


htmlToString : Html msg -> Result String String
htmlToString html =
    Test.Html.Internal.Inert.fromHtml html
        |> Result.map
            (Test.Html.Internal.Inert.toElmHtml
                >> Test.Html.Query.Internal.prettyPrint
            )


{-| Upload snapshots to Percy.io for visual regression testing. You'll need to create an account first in order to get an API key.

    import Effect.Snapshot exposing (PercyApiKey(..))
    import List.Nonempty exposing (Nonempty(..))
    import MyLogin

    heroBannerBytes =
        ...

    a =
        uploadSnapshots
            { apiKey = PercyApiKey "my api token"
            , gitBranch = "my-feature-branch"
            , gitTargetBranch = "main"
            , snapshots = Nonempty { name = "Login page", html = MyLogin.view } []
            , publicFiles = [ { filepath = "hero-banner.png", content = heroBannerBytes } ]
            }

-}
uploadSnapshots :
    { apiKey : PercyApiKey
    , gitBranch : String
    , gitTargetBranch : String
    , snapshots : Nonempty (Snapshot msg)
    , publicFiles : List PublicFiles
    }
    -> Task Http.Error FinalizeResponse
uploadSnapshots { apiKey, gitBranch, gitTargetBranch, snapshots, publicFiles } =
    let
        publicFiles_ =
            List.map (\file -> ( SHA256.fromBytes file.content, file )) publicFiles
    in
    createBuild
        apiKey
        { attributes =
            { branch = gitBranch
            , targetBranch = gitTargetBranch
            }
        , relationships = { resources = { data = [] } }
        }
        |> Task.andThen
            (\{ data } ->
                List.Nonempty.toList snapshots
                    |> List.map
                        (\snapshot_ ->
                            let
                                hash : SHA256.Digest
                                hash =
                                    SHA256.fromString htmlString

                                htmlString : String
                                htmlString =
                                    "<!DOCTYPE html>\n<html><head></head><body>"
                                        ++ Result.withDefault
                                            "Something went wrong when converting this Html into a String. Please file a github issue with what the Html looked like."
                                            (htmlToString snapshot_.html)
                                        ++ "</body></html>"

                                filesToUpload =
                                    List.filter
                                        (\( _, file ) -> String.contains file.filepath htmlString)
                                        publicFiles_
                            in
                            createSnapshot
                                apiKey
                                data.buildId
                                { name = snapshot_.name
                                , widths = Nonempty 500 []
                                , minHeight = Nothing
                                , resources =
                                    Nonempty
                                        { id = hash
                                        , attributes =
                                            { resourceUrl = "/index.html"
                                            , isRoot = True
                                            , mimeType = Just "text/html"
                                            }
                                        }
                                        (List.map
                                            (\( fileHash, file ) ->
                                                { id = fileHash
                                                , attributes =
                                                    { resourceUrl =
                                                        List.map Url.percentEncode (String.split "/" file.filepath)
                                                            |> String.join "/"
                                                            |> (++) "/"
                                                    , isRoot = False
                                                    , mimeType = Nothing
                                                    }
                                                }
                                            )
                                            filesToUpload
                                        )
                                }
                                |> Task.andThen
                                    (\_ ->
                                        uploadResource apiKey data.buildId hash htmlString
                                            :: List.map
                                                (\( fileHash, file ) ->
                                                    uploadResourceBytes apiKey data.buildId fileHash file.content
                                                )
                                                filesToUpload
                                            |> Task.sequence
                                    )
                        )
                    |> Task.sequence
                    |> Task.andThen (\_ -> finalize apiKey data.buildId)
            )


type alias SnapshotResource =
    { id : SHA256.Digest
    , attributes :
        { -- resource filepath (probably the filepath that will be used within the webpage)
          resourceUrl : String
        , -- True if this is the html text
          isRoot : Bool
        , mimeType : Maybe String
        }
    }


percyApiDomain : String
percyApiDomain =
    "https://percy.io/api/v1"


uploadResource : PercyApiKey -> BuildId -> SHA256.Digest -> String -> Task Http.Error ()
uploadResource (PercyApiKey apiKey) (BuildId buildId) hash content =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Token " ++ apiKey) ]
        , url = Url.Builder.crossOrigin percyApiDomain [ "builds", buildId, "resources" ] []
        , body = Http.jsonBody (encodeUploadResource hash content)
        , resolver = Http.stringResolver (resolver (Json.Decode.succeed ()))
        , timeout = Nothing
        }


uploadResourceBytes : PercyApiKey -> BuildId -> SHA256.Digest -> Bytes -> Task Http.Error ()
uploadResourceBytes (PercyApiKey apiKey) (BuildId buildId) hash content =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Token " ++ apiKey) ]
        , url = Url.Builder.crossOrigin percyApiDomain [ "builds", buildId, "resources" ] []
        , body = Http.jsonBody (encodeUploadResourceBytes hash content)
        , resolver = Http.stringResolver (resolver (Json.Decode.succeed ()))
        , timeout = Nothing
        }


encodeUploadResource : SHA256.Digest -> String -> Json.Encode.Value
encodeUploadResource hash content =
    Json.Encode.object
        [ ( "data"
          , Json.Encode.object
                [ ( "type", Json.Encode.string "resources" )
                , ( "id", SHA256.toHex hash |> Json.Encode.string )
                , ( "attributes"
                  , Json.Encode.object
                        [ ( "base64-content"
                          , Base64.fromString content |> Maybe.withDefault "" |> Json.Encode.string
                          )
                        ]
                  )
                ]
          )
        ]


encodeUploadResourceBytes : SHA256.Digest -> Bytes -> Json.Encode.Value
encodeUploadResourceBytes hash content =
    Json.Encode.object
        [ ( "data"
          , Json.Encode.object
                [ ( "type", Json.Encode.string "resources" )
                , ( "id", SHA256.toHex hash |> Json.Encode.string )
                , ( "attributes"
                  , Json.Encode.object
                        [ ( "base64-content"
                          , Base64.fromBytes content |> Maybe.withDefault "" |> Json.Encode.string
                          )
                        ]
                  )
                ]
          )
        ]


createSnapshot :
    PercyApiKey
    -> BuildId
    ->
        { name : String
        , widths : Nonempty Int
        , minHeight : Maybe Int
        , resources : Nonempty SnapshotResource
        }
    -> Task Http.Error ()
createSnapshot (PercyApiKey apiKey) (BuildId buildId) data =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Token " ++ apiKey) ]
        , url = Url.Builder.crossOrigin percyApiDomain [ "builds", buildId, "snapshots" ] []
        , body = Http.jsonBody (encodeCreateSnapshot data)
        , resolver = Http.stringResolver (resolver (Json.Decode.succeed ()))
        , timeout = Nothing
        }


encodeCreateSnapshot :
    { name : String
    , widths : Nonempty Int
    , minHeight : Maybe Int
    , resources : Nonempty SnapshotResource
    }
    -> Json.Encode.Value
encodeCreateSnapshot data =
    Json.Encode.object
        [ ( "data"
          , Json.Encode.object
                [ ( "type", Json.Encode.string "snapshots" )
                , ( "attributes"
                  , Json.Encode.object
                        [ ( "name", Json.Encode.string data.name )
                        , ( "widths"
                          , List.Nonempty.toList data.widths
                                |> List.map (clamp 10 2000)
                                |> Json.Encode.list Json.Encode.int
                          )
                        , ( "minimum-height"
                          , case data.minHeight of
                                Just minHeight ->
                                    Json.Encode.int (clamp 10 2000 minHeight)

                                Nothing ->
                                    Json.Encode.null
                          )
                        ]
                  )
                , ( "relationships"
                  , Json.Encode.object
                        [ ( "resources"
                          , Json.Encode.object
                                [ ( "data"
                                  , Json.Encode.list encodeResource (List.Nonempty.toList data.resources)
                                  )
                                ]
                          )
                        ]
                  )
                ]
          )
        ]


{-| An API key needed to upload snapshots to Percy.io. Create an account first in order to get an API key.
-}
type PercyApiKey
    = PercyApiKey String


type alias BuildData =
    { attributes :
        { branch : String
        , targetBranch : String
        }
    , relationships :
        { resources :
            { data : List SnapshotResource }
        }
    }


type BuildId
    = BuildId String


type alias BuildResponse =
    { data : { buildId : BuildId }
    }


buildResponseCodec : Decoder BuildResponse
buildResponseCodec =
    Json.Decode.map BuildResponse
        (Json.Decode.field
            "data"
            (Json.Decode.map (\id -> { buildId = id })
                (Json.Decode.field "id" buildIdCodec)
            )
        )


buildIdCodec : Decoder BuildId
buildIdCodec =
    Json.Decode.string |> Json.Decode.map BuildId


encodeBuildData : BuildData -> Json.Encode.Value
encodeBuildData buildData =
    Json.Encode.object
        [ ( "data"
          , Json.Encode.object
                [ ( "type", Json.Encode.string "builds" )
                , ( "attributes"
                  , Json.Encode.object
                        [ ( "branch", Json.Encode.string buildData.attributes.branch )
                        , ( "target-branch", Json.Encode.string buildData.attributes.targetBranch )
                        ]
                  )
                , ( "relationships"
                  , Json.Encode.object
                        [ ( "resources"
                          , Json.Encode.object
                                [ ( "data"
                                  , Json.Encode.list encodeResource buildData.relationships.resources.data
                                  )
                                ]
                          )
                        ]
                  )
                ]
          )
        ]


encodeResource : SnapshotResource -> Json.Encode.Value
encodeResource resource =
    Json.Encode.object
        [ ( "type", Json.Encode.string "resources" )
        , ( "id", SHA256.toHex resource.id |> Json.Encode.string )
        , ( "attributes"
          , Json.Encode.object
                [ ( "resource-url", Json.Encode.string resource.attributes.resourceUrl )
                , ( "is-root", Json.Encode.bool resource.attributes.isRoot )
                , ( "mimetype", encodeMaybe Json.Encode.string resource.attributes.mimeType )
                ]
          )
        ]


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe encoder maybe =
    case maybe of
        Just value ->
            encoder value

        Nothing ->
            Json.Encode.null


createBuild : PercyApiKey -> BuildData -> Task Http.Error BuildResponse
createBuild (PercyApiKey apiKey) buildData =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Token " ++ apiKey) ]
        , url = Url.Builder.crossOrigin percyApiDomain [ "builds" ] []
        , body = Http.jsonBody (encodeBuildData buildData)
        , resolver = Http.stringResolver (resolver buildResponseCodec)
        , timeout = Nothing
        }


resolver : Decoder a -> Http.Response String -> Result Http.Error a
resolver codec =
    \response ->
        case response of
            Http.BadUrl_ url ->
                Http.BadUrl url |> Err

            Http.Timeout_ ->
                Err Http.Timeout

            Http.NetworkError_ ->
                Err Http.NetworkError

            Http.BadStatus_ metadata _ ->
                Http.BadStatus metadata.statusCode |> Err

            Http.GoodStatus_ _ body ->
                case Json.Decode.decodeString codec body of
                    Ok ok ->
                        Ok ok

                    Err error ->
                        Json.Decode.errorToString error |> Http.BadBody |> Err


finalize : PercyApiKey -> BuildId -> Task Http.Error FinalizeResponse
finalize (PercyApiKey apiKey) (BuildId buildId) =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Token " ++ apiKey) ]
        , url = Url.Builder.crossOrigin percyApiDomain [ "builds", buildId, "finalize" ] []
        , body = Http.emptyBody
        , resolver = Http.stringResolver (resolver finalizeResponseCodec)
        , timeout = Nothing
        }


type alias FinalizeResponse =
    { success : Bool }


finalizeResponseCodec : Decoder FinalizeResponse
finalizeResponseCodec =
    Json.Decode.map FinalizeResponse
        (Json.Decode.field "success" Json.Decode.bool)
