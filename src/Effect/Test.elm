module Effect.Test exposing
    ( start, Config, connectFrontend, FrontendApp, BackendApp, HttpRequest, HttpResponse(..), RequestedBy(..), PortToJs, FileData, FileUpload(..), MultipleFilesUpload(..), uploadBytesFile, uploadStringFile, Data, FileContents(..)
    , FrontendActions, sendToBackend, backendUpdate, simulateTime, fastForward, andThen, Instructions, startTime, HttpBody(..), HttpPart(..)
    , checkState, checkBackend, toTest, toSnapshots
    , fakeNavigationKey, viewer, Msg, Model, viewerWith, ViewerWith, startViewer, addStringFile, addBytesFile, addTexture, addTextureWithOptions
    , startHeadless, HeadlessMsg
    )

{-|


## Setting up end to end tests

@docs start, Config, connectFrontend, FrontendApp, BackendApp, HttpRequest, HttpResponse, RequestedBy, PortToJs, FileData, FileUpload, MultipleFilesUpload, uploadBytesFile, uploadStringFile, Data, FileContents


## Control the tests

@docs FrontendActions, sendToBackend, backendUpdate, simulateTime, fastForward, andThen, Instructions, startTime, HttpBody, HttpPart


## Check the current state

@docs checkState, checkBackend, toTest, toSnapshots


## Test viewer

Sometimes it's hard to tell what's going on in an end to end test. One way to make this easier to use the `viewer` function. It's like a test runner for your browser that also lets you see the frontend of an app as simulated inputs are being triggered.

@docs fakeNavigationKey, viewer, Msg, Model, viewerWith, ViewerWith, startViewer, addStringFile, addBytesFile, addTexture, addTextureWithOptions


## Test runner

If you want to just run the end to end tests to make sure they work, or automatically check that they pass in your CI pipeline then you can setup a headless test runner with this function:

@docs startHeadless, HeadlessMsg

-}

import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import Base64
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode
import Bytes.Encode
import DebugParser exposing (ElmValue(..), ExpandableValue(..), SequenceType(..))
import Dict as RegularDict
import Duration exposing (Duration)
import Effect.Browser.Dom exposing (HtmlId)
import Effect.Browser.Navigation
import Effect.Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Body)
import Effect.Internal exposing (Command(..), File, NavigationKey(..), Task(..))
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Snapshot exposing (Snapshot)
import Effect.Subscription exposing (Subscription)
import Effect.TreeView exposing (CollapsedField(..), PathNode)
import Effect.WebGL.Texture
import Expect exposing (Expectation)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Http
import Json.Decode
import Json.Encode
import List.Nonempty exposing (Nonempty)
import Math.Matrix4 as Mat4
import Process
import Quantity
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Test exposing (Test)
import Test.Html.Event
import Test.Html.Query
import Test.Html.Selector
import Test.Runner
import Time
import Url exposing (Url)
import WebGLFix.Texture


{-| Configure the end to end test before starting it

    import Backend
    import Effect.Test
    import Frontend
    import Test exposing (Test)
    import Url

    config =
        { frontendApp = Frontend.appFunctions
        , backendApp = Backend.appFunctions
        , handleHttpRequest = always Effect.Test.UnhandledHttpRequest
        , handlePortToJs = always Nothing
        , handleFileUpload = always Effect.Test.UnhandledFileUpload
        , handleMultipleFilesUpload = always Effect.Test.UnhandledMultiFileUpload
        , domain = unsafeUrl "https://my-app.lamdera.app"
        }

    test : Test
    test =
        Effect.Test.start "myButton is clickable"
            |> Effect.Test.connectFrontend
                sessionId0
                myDomain
                { width = 1920, height = 1080 }
                (\( state, frontendActions ) ->
                    state
                        |> frontendActions.clickButton { htmlId = "myButton" }
                )
            |> Effect.Test.toTest

    unsafeUrl : String -> Url
    unsafeUrl urlText =
        case Url.fromString urlText of
            Just url ->
                url

            Nothing ->
                unsafeUrl urlText

-}
type alias Config toBackend frontendMsg frontendModel toFrontend backendMsg backendModel =
    { frontendApp : FrontendApp toBackend frontendMsg frontendModel toFrontend
    , backendApp : BackendApp toBackend toFrontend backendMsg backendModel
    , handleHttpRequest : { data : Data frontendModel backendModel, currentRequest : HttpRequest } -> HttpResponse
    , handlePortToJs : { data : Data frontendModel backendModel, currentRequest : PortToJs } -> Maybe ( String, Json.Decode.Value )
    , handleFileUpload : { data : Data frontendModel backendModel, mimeTypes : List String } -> FileUpload
    , handleMultipleFilesUpload : { data : Data frontendModel backendModel, mimeTypes : List String } -> MultipleFilesUpload
    , domain : Url
    }


{-| Possible simulated user actions for when `Effect.File.Select.file` is triggered.
-}
type FileUpload
    = CancelFileUpload
    | UploadFile FileData
    | UnhandledFileUpload


{-| File data for when simulating a user uploading a file via `Effect.File.Select.file` or `Effect.File.Select.files`
-}
type FileData
    = FileUploadData { name : String, mimeType : String, content : Effect.Internal.FileUploadContent, lastModified : Time.Posix }


{-| Create a file upload containing text data
-}
uploadStringFile : String -> String -> String -> Time.Posix -> FileData
uploadStringFile name mimeType content lastModified =
    FileUploadData
        { name = name
        , mimeType = mimeType
        , content = Effect.Internal.StringFile content
        , lastModified = lastModified
        }


{-| Create a file upload containing binary data
-}
uploadBytesFile : String -> String -> Bytes -> Time.Posix -> FileData
uploadBytesFile name mimeType content lastModified =
    FileUploadData
        { name = name
        , mimeType = mimeType
        , content = Effect.Internal.BytesFile content
        , lastModified = lastModified
        }


{-| Possible simulated user actions for when `Effect.File.Select.files` is triggered.
-}
type MultipleFilesUpload
    = CancelMultipleFilesUpload
    | UploadMultipleFiles FileData (List FileData)
    | UnhandledMultiFileUpload


type alias ToBackendData toBackend =
    { sessionId : SessionId
    , clientId : ClientId
    , toBackend : toBackend
    , stepIndex : Maybe Int
    }


type alias BackendPendingEffect toFrontend backendMsg =
    { cmds : Command BackendOnly toFrontend backendMsg
    , stepIndex : Int
    }


type alias FrontendPendingEffect toBackend frontendMsg =
    { cmds : Command FrontendOnly toBackend frontendMsg
    , stepIndex : Int
    }


type alias State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel =
    { testName : String
    , frontendApp : FrontendApp toBackend frontendMsg frontendModel toFrontend
    , backendApp : BackendApp toBackend toFrontend backendMsg backendModel
    , model : backendModel
    , history : Array (Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    , pendingEffects : Array (BackendPendingEffect toFrontend backendMsg)
    , frontends : Dict ClientId (FrontendState toBackend frontendMsg frontendModel toFrontend)
    , counter : Int
    , elapsedTime : Duration
    , toBackend : List (ToBackendData toBackend)
    , timers : Dict Duration { startTime : Time.Posix }
    , testErrors : List TestError
    , httpRequests : List HttpRequest
    , handleHttpRequest : { data : Data frontendModel backendModel, currentRequest : HttpRequest } -> HttpResponse
    , handlePortToJs :
        { currentRequest : PortToJs, data : Data frontendModel backendModel }
        -> Maybe ( String, Json.Decode.Value )
    , portRequests : List PortToJs
    , handleFileUpload : { data : Data frontendModel backendModel, mimeTypes : List String } -> FileUpload
    , handleMultipleFilesUpload : { data : Data frontendModel backendModel, mimeTypes : List String } -> MultipleFilesUpload
    , domain : Url
    , snapshots : List { name : String, body : List (Html frontendMsg), width : Int, height : Int }
    , downloads : List { filename : String, mimeType : String, content : FileContents, downloadedAt : Time.Posix }
    }


{-| -}
type alias Data frontendModel backendModel =
    { httpRequests : List HttpRequest
    , portRequests : List PortToJs
    , time : Time.Posix
    , backend : backendModel
    , frontends : Dict ClientId frontendModel
    , downloads : List { filename : String, mimeType : String, content : FileContents, downloadedAt : Time.Posix }
    }


stateToData : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> Data frontendModel backendModel
stateToData state =
    { httpRequests = state.httpRequests
    , portRequests = state.portRequests
    , time = currentTime state
    , backend = state.model
    , frontends = Dict.map (\_ frontend -> frontend.model) state.frontends
    , downloads = state.downloads
    }


{-| -}
type FileContents
    = StringFile String
    | BytesFile Bytes


{-| -}
type alias PortToJs =
    { clientId : ClientId, portName : String, value : Json.Encode.Value }


{-| -}
type alias HttpRequest =
    { requestedBy : RequestedBy
    , method : String
    , url : String
    , body : HttpBody
    , headers : List ( String, String )
    , sentAt : Time.Posix
    }


{-| The response for an http request.
Note that if the http request was expecting one form of data (for example json) and the response contains a different type of data (for example a String) then the data will automatically be converted.
The exception to this is Texture. If the request is expecting a Texture (this will only happen with `Effect.WebGL.Texture.load` and `Effect.WebGL.Texture.loadWith`) then the response has to contain a Texture if you want the request to succeed.
In other words, sending the Bytes that represent that texture won't work.
-}
type HttpResponse
    = BadUrlResponse String
    | TimeoutResponse
    | NetworkErrorResponse
    | BadStatusResponse Effect.Http.Metadata String
    | BytesHttpResponse Effect.Http.Metadata Bytes
    | StringHttpResponse Effect.Http.Metadata String
    | JsonHttpResponse Effect.Http.Metadata Json.Encode.Value
    | TextureHttpResponse Effect.Http.Metadata Effect.WebGL.Texture.Texture
    | UnhandledHttpRequest


{-| Who made this http request?
-}
type RequestedBy
    = RequestedByFrontend ClientId
    | RequestedByBackend


{-| Only use this for tests!
-}
fakeNavigationKey : Effect.Browser.Navigation.Key
fakeNavigationKey =
    Effect.Browser.Navigation.fromInternalKey Effect.Internal.MockNavigationKey


httpBodyFromInternal : Effect.Internal.HttpBody -> HttpBody
httpBodyFromInternal body =
    case body of
        Effect.Internal.EmptyBody ->
            EmptyBody

        Effect.Internal.StringBody record ->
            StringBody record

        Effect.Internal.JsonBody value ->
            JsonBody value

        Effect.Internal.MultipartBody httpParts ->
            List.map httpPartFromInternal httpParts |> MultipartBody

        Effect.Internal.BytesBody string bytes ->
            BytesBody string bytes

        Effect.Internal.FileBody file ->
            FileBody file


{-| -}
type HttpBody
    = EmptyBody
    | StringBody { contentType : String, content : String }
    | JsonBody Json.Encode.Value
    | MultipartBody (List HttpPart)
    | BytesBody String Bytes
    | FileBody File


httpPartFromInternal part =
    case part of
        Effect.Internal.StringPart a b ->
            StringPart a b

        Effect.Internal.FilePart string file ->
            FilePart string file

        Effect.Internal.BytesPart key mimeType bytes ->
            BytesPart { key = key, mimeType = mimeType, content = bytes }


type TestError
    = CustomError String
    | ClientIdNotFound ClientId
    | ViewTestError String
    | InvalidUrl String
    | FileUploadNotHandled
    | MultipleFilesUploadNotHandled
    | HttpResponseContainsBytesThatCantConvertToString HttpRequest
    | HttpResponseCantConvertTextureToString HttpRequest
    | HttpRequestNotHandled HttpRequest


{-| -}
type HttpPart
    = StringPart String String
    | FilePart String File
    | BytesPart { key : String, mimeType : String, content : Bytes }


{-| -}
type Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    = NextStep (State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel) (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    | AndThen (State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel) (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    | Start (State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)


{-| Make the test fail if it doesn't seem some condition.
-}
checkState :
    (Data frontendModel backendModel -> Result String ())
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
checkState checkFunc =
    NextStep
        (\state ->
            case checkFunc (stateToData state) of
                Ok () ->
                    state
                        |> addEvent (CheckStateEvent { checkType = CheckState, isSuccessful = True })

                Err error ->
                    addTestError (CustomError error) state
                        |> addEvent (CheckStateEvent { checkType = CheckState, isSuccessful = False })
        )


{-| Make the test fail if the backend model doesn't seem some condition.
-}
checkBackend :
    (backendModel -> Result String ())
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
checkBackend checkFunc =
    NextStep
        (\state ->
            case checkFunc state.model of
                Ok () ->
                    state |> addEvent (CheckStateEvent { checkType = CheckBackend, isSuccessful = True })

                Err error ->
                    addTestError (CustomError error) state
                        |> addEvent (CheckStateEvent { checkType = CheckBackend, isSuccessful = False })
        )


{-| -}
checkFrontend :
    ClientId
    -> (frontendModel -> Result String ())
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
checkFrontend clientId checkFunc =
    NextStep
        (\state ->
            case Dict.get clientId state.frontends of
                Just frontend ->
                    case checkFunc frontend.model of
                        Ok () ->
                            state
                                |> addEvent (CheckStateEvent { checkType = CheckFrontendState clientId, isSuccessful = True })

                        Err error ->
                            addTestError (CustomError error) state
                                |> addEvent (CheckStateEvent { checkType = CheckFrontendState clientId, isSuccessful = False })

                Nothing ->
                    addTestError (ClientIdNotFound clientId) state
                        |> addEvent (CheckStateEvent { checkType = CheckFrontendState clientId, isSuccessful = False })
        )


addTestError :
    TestError
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
addTestError error state =
    { state | testErrors = state.testErrors ++ [ error ] }


checkView :
    ClientId
    -> (Test.Html.Query.Single frontendMsg -> Expectation)
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
checkView clientId query =
    NextStep
        (\state ->
            case Dict.get clientId state.frontends of
                Just frontend ->
                    case
                        state.frontendApp.view frontend.model
                            |> .body
                            |> Html.div []
                            |> Test.Html.Query.fromHtml
                            |> query
                            |> Test.Runner.getFailureReason
                    of
                        Just { description } ->
                            addTestError (ViewTestError description) state
                                |> addEvent (CheckStateEvent { checkType = CheckFrontendView clientId, isSuccessful = False })

                        Nothing ->
                            state |> addEvent (CheckStateEvent { checkType = CheckFrontendView clientId, isSuccessful = True })

                Nothing ->
                    addTestError (ClientIdNotFound clientId) state
                        |> addEvent (CheckStateEvent { checkType = CheckFrontendView clientId, isSuccessful = False })
        )


frontendUpdate :
    ClientId
    -> frontendMsg
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
frontendUpdate clientId msg =
    let
        event : EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        event =
            TestEvent (Just clientId) ("Trigger frontend update: " ++ Debug.toString msg)
    in
    NextStep
        (\state ->
            if Dict.member clientId state.frontends then
                addEvent event state
                    |> handleFrontendUpdate clientId (currentTime state) msg

            else
                addTestError (ClientIdNotFound clientId) state
                    |> addEvent event
        )


testErrorToString : TestError -> String
testErrorToString error =
    case error of
        CustomError text_ ->
            text_

        ClientIdNotFound clientId ->
            "Client Id " ++ Effect.Lamdera.clientIdToString clientId ++ " not found"

        ViewTestError string ->
            if String.length string > 100 then
                String.left 100 string ++ "..."

            else
                string

        InvalidUrl string ->
            string ++ " is not a valid url"

        FileUploadNotHandled ->
            "A client tried uploading a file but it wasn't handled by Config.handleFileUpload"

        MultipleFilesUploadNotHandled ->
            "A client tried uploading multiple files but it wasn't handled by Config.multipleFilesUpload"

        HttpResponseContainsBytesThatCantConvertToString httpRequest ->
            "Config.handleHttpRequest returned Bytes to " ++ httpRequest.url ++ " but a String was expected and the Bytes couldn't be converted into a valid UTF-8 string"

        HttpResponseCantConvertTextureToString httpRequest ->
            "Config.handleHttpRequest returned a Texture to " ++ httpRequest.url ++ " but a String was expected"

        HttpRequestNotHandled httpRequest ->
            "A client tried making an http request to " ++ httpRequest.url ++ " but it wasn't handled by Config.handleHttpRequest"


{-| -}
toTest : Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> Test
toTest instructions =
    let
        state =
            instructionsToState instructions
    in
    Test.test state.testName <|
        \() ->
            case state.testErrors of
                firstError :: _ ->
                    testErrorToString firstError |> Expect.fail

                [] ->
                    let
                        duplicates =
                            gatherEqualsBy .name state.snapshots
                                |> List.filterMap
                                    (\( first, rest ) ->
                                        if List.isEmpty rest then
                                            Nothing

                                        else
                                            Just ( first.name, List.length rest + 1 )
                                    )
                    in
                    case duplicates of
                        [] ->
                            Expect.pass

                        ( name, count ) :: [] ->
                            "A snapshot named \""
                                ++ name
                                ++ "\" appears "
                                ++ String.fromInt count
                                ++ " times. Make sure snapshot names are unique!"
                                |> Expect.fail

                        rest ->
                            "These snapshot names appear multiple times:"
                                ++ String.concat
                                    (List.map
                                        (\( name, count ) -> "\n" ++ name ++ " (" ++ String.fromInt count ++ " times)")
                                        rest
                                    )
                                ++ " Make sure snapshot names are unique!"
                                |> Expect.fail


{-| Copied from elm-community/list-extra

Group equal elements together. A function is applied to each element of the list
and then the equality check is performed against the results of that function evaluation.
Elements will be grouped in the same order as they appear in the original list. The
same applies to elements within each group.
gatherEqualsBy .age [{age=25},{age=23},{age=25}]
--> [({age=25},[{age=25}]),({age=23},[])]

-}
gatherEqualsBy : (a -> b) -> List a -> List ( a, List a )
gatherEqualsBy extract list =
    gatherWith (\a b -> extract a == extract b) list


{-| Copied from elm-community/list-extra

Group equal elements together using a custom equality function. Elements will be
grouped in the same order as they appear in the original list. The same applies to
elements within each group.
gatherWith (==) [1,2,1,3,2]
--> [(1,[1]),(2,[2]),(3,[])]

-}
gatherWith : (a -> a -> Bool) -> List a -> List ( a, List a )
gatherWith testFn list =
    let
        helper : List a -> List ( a, List a ) -> List ( a, List a )
        helper scattered gathered =
            case scattered of
                [] ->
                    List.reverse gathered

                toGather :: population ->
                    let
                        ( gathering, remaining ) =
                            List.partition (testFn toGather) population
                    in
                    helper remaining (( toGather, gathering ) :: gathered)
    in
    helper list []


{-| Get all snapshots from a test.
This can be used with Effect.Snapshot.uploadSnapshots to perform visual regression testing.
-}
toSnapshots :
    Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> List (Snapshot frontendMsg)
toSnapshots instructions =
    let
        state =
            instructionsToState instructions
    in
    state
        |> .snapshots
        |> List.map
            (\{ name, body, width, height } ->
                { name = state.testName ++ ": " ++ name
                , body = body
                , widths = List.Nonempty.fromElement width
                , minimumHeight = Just height
                }
            )


instructionsToState :
    Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
instructionsToState inProgress =
    case inProgress of
        NextStep stateFunc inProgress_ ->
            instructionsToState inProgress_ |> stateFunc

        AndThen stateFunc inProgress_ ->
            instructionsToState inProgress_ |> stateFunc |> instructionsToState

        Start state ->
            state


type alias FrontendState toBackend frontendMsg frontendModel toFrontend =
    { model : frontendModel
    , sessionId : SessionId
    , pendingEffects : Array (FrontendPendingEffect toBackend frontendMsg)
    , toFrontend : List (ToFrontendData toFrontend)
    , timers : Dict Duration { startTime : Time.Posix }
    , url : Url
    , windowSize : { width : Int, height : Int }
    }


{-| -}
startTime : Time.Posix
startTime =
    Time.millisToPosix 0


{-| -}
type alias FrontendApp toBackend frontendMsg frontendModel toFrontend =
    { init : Url -> Effect.Browser.Navigation.Key -> ( frontendModel, Command FrontendOnly toBackend frontendMsg )
    , onUrlRequest : UrlRequest -> frontendMsg
    , onUrlChange : Url -> frontendMsg
    , update : frontendMsg -> frontendModel -> ( frontendModel, Command FrontendOnly toBackend frontendMsg )
    , updateFromBackend : toFrontend -> frontendModel -> ( frontendModel, Command FrontendOnly toBackend frontendMsg )
    , view : frontendModel -> Browser.Document frontendMsg
    , subscriptions : frontendModel -> Subscription FrontendOnly frontendMsg
    }


{-| -}
type alias BackendApp toBackend toFrontend backendMsg backendModel =
    { init : ( backendModel, Command BackendOnly toFrontend backendMsg )
    , update : backendMsg -> backendModel -> ( backendModel, Command BackendOnly toFrontend backendMsg )
    , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Command BackendOnly toFrontend backendMsg )
    , subscriptions : backendModel -> Subscription BackendOnly backendMsg
    }


{-| FrontendActions contains the possible functions we can call on the client we just connected.

    import Effect.Test
    import Test exposing (Test)
    import Url

    config =
        { frontendApp = Frontend.appFunctions
        , backendApp = Backend.appFunctions
        , handleHttpRequest = always Effect.Test.UnhandledHttpRequest
        , handlePortToJs = always Nothing
        , handleFileUpload = always Effect.Test.UnhandledFileUpload
        , handleMultipleFilesUpload = always Effect.Test.UnhandledMultiFileUpload
        , domain = unsafeUrl "https://my-app.lamdera.app"
        }

    test : Test
    test =
        Effect.Test.start "myButton is clickable"
            |> Effect.Test.connectFrontend
                sessionId0
                myDomain
                { width = 1920, height = 1080 }
                (\( state, frontendActions ) ->
                    -- frontendActions is a record we can use on this specific frontend we just connected
                    state
                        |> frontendActions.clickButton { htmlId = "myButton" }
                )
            |> Effect.Test.toTest

    unsafeUrl : String -> Url
    unsafeUrl urlText =
        case Url.fromString urlText of
            Just url ->
                url

            Nothing ->
                unsafeUrl urlText

-}
type alias FrontendActions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel =
    { clientId : ClientId
    , keyDownEvent :
        HtmlId
        -> { keyCode : Int }
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , clickButton :
        HtmlId
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , inputText :
        HtmlId
        -> String
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , clickLink :
        { href : String }
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , checkView :
        (Test.Html.Query.Single frontendMsg -> Expectation)
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , update :
        frontendMsg
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , snapshotView :
        { name : String }
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    }


{-| Start a end-to-end test

    import Backend
    import Effect.Test
    import Frontend
    import Test exposing (Test)
    import Url

    config =
        { frontendApp = Frontend.appFunctions
        , backendApp = Backend.appFunctions
        , handleHttpRequest = always Effect.Test.NetworkErrorResponse
        , handlePortToJs = always Nothing
        , handleFileUpload = always Effect.Test.UnhandledFileUpload
        , handleMultipleFilesUpload = always Effect.Test.UnhandledMultiFileUpload
        , domain = unsafeUrl "https://my-app.lamdera.app"
        }

    test : Test
    test =
        Effect.Test.start "myButton is clickable"
            |> Effect.Test.connectFrontend
                sessionId0
                myDomain
                { width = 1920, height = 1080 }
                (\( state, frontendActions ) ->
                    state
                        |> frontendActions.clickButton { htmlId = "myButton" }
                )
            |> Effect.Test.toTest

    unsafeUrl : String -> Url
    unsafeUrl urlText =
        case Url.fromString urlText of
            Just url ->
                url

            Nothing ->
                unsafeUrl urlText

-}
start :
    Config toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> String
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
start config testName =
    let
        ( backend, cmd ) =
            config.backendApp.init

        state : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        state =
            { testName = testName
            , frontendApp = config.frontendApp
            , backendApp = config.backendApp
            , model = backend
            , history = Array.empty
            , pendingEffects = Array.fromList [ { cmds = cmd, stepIndex = 0 } ]
            , frontends = Dict.empty
            , counter = 0
            , elapsedTime = Quantity.zero
            , toBackend = []
            , timers = getTimers (config.backendApp.subscriptions backend) |> Dict.map (\_ _ -> { startTime = startTime })
            , testErrors = []
            , httpRequests = []
            , handleHttpRequest = config.handleHttpRequest
            , handlePortToJs = config.handlePortToJs
            , portRequests = []
            , handleFileUpload = config.handleFileUpload
            , handleMultipleFilesUpload = config.handleMultipleFilesUpload
            , domain = config.domain
            , snapshots = []
            , downloads = []
            }
                |> addEvent (BackendInitEvent cmd)
    in
    Start state


getTimers : Subscription restriction backendMsg -> Dict Duration { msg : Nonempty (Time.Posix -> backendMsg) }
getTimers backendSub =
    case backendSub of
        Effect.Internal.SubBatch batch ->
            List.foldl
                (\sub dict ->
                    Dict.foldl
                        (\duration value dict2 ->
                            Dict.update
                                duration
                                (\maybe ->
                                    (case maybe of
                                        Just data ->
                                            { msg = List.Nonempty.append value.msg data.msg }

                                        Nothing ->
                                            value
                                    )
                                        |> Just
                                )
                                dict2
                        )
                        dict
                        (getTimers sub)
                )
                Dict.empty
                batch

        Effect.Internal.TimeEvery duration msg ->
            Dict.singleton duration { msg = List.Nonempty.singleton msg }

        Effect.Internal.OnAnimationFrame msg ->
            Dict.singleton animationFrame { msg = List.Nonempty.singleton msg }

        Effect.Internal.OnAnimationFrameDelta msg ->
            Dict.singleton animationFrame { msg = List.Nonempty.singleton (\_ -> msg animationFrame) }

        _ ->
            Dict.empty


{-| Directly trigger an update on the backend. Normally you shouldn't need to do this as things like Time.every and completed tasks will generate updates automatically.
-}
backendUpdate :
    backendMsg
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
backendUpdate backendMsg instructions =
    NextStep
        (\state ->
            handleBackendUpdate
                (currentTime state)
                state.backendApp
                backendMsg
                (addEvent (TestEvent Nothing ("Trigger BackendMsg: " ++ Debug.toString backendMsg)) state)
        )
        instructions


getClientDisconnectSubs : Effect.Internal.Subscription BackendOnly backendMsg -> List (SessionId -> ClientId -> backendMsg)
getClientDisconnectSubs backendSub =
    case backendSub of
        Effect.Internal.SubBatch batch ->
            List.foldl (\sub list -> getClientDisconnectSubs sub ++ list) [] batch

        Effect.Internal.OnDisconnect msg ->
            [ \sessionId clientId ->
                msg
                    (Effect.Lamdera.sessionIdToString sessionId |> Effect.Internal.SessionId)
                    (Effect.Lamdera.clientIdToString clientId |> Effect.Internal.ClientId)
            ]

        _ ->
            []


getClientConnectSubs : Effect.Internal.Subscription BackendOnly backendMsg -> List (SessionId -> ClientId -> backendMsg)
getClientConnectSubs backendSub =
    case backendSub of
        Effect.Internal.SubBatch batch ->
            List.foldl (\sub list -> getClientConnectSubs sub ++ list) [] batch

        Effect.Internal.OnConnect msg ->
            [ \sessionId clientId ->
                msg
                    (Effect.Lamdera.sessionIdToString sessionId |> Effect.Internal.SessionId)
                    (Effect.Lamdera.clientIdToString clientId |> Effect.Internal.ClientId)
            ]

        _ ->
            []


{-| Add a frontend client to the end to end test

    import Effect.Test
    import Test exposing (Test)
    import Url

    testApp =
        Effect.Test.testApp
            Frontend.appFunctions
            Backend.appFunctions
            (always NetworkError_)
            (always Nothing)
            (always Nothing)
            (unsafeUrl "https://my-app.lamdera.app")

    test : Test
    test =
        testApp "myButton is clickable"
            |> Effect.Test.connectFrontend
                sessionId0
                myDomain
                { width = 1920, height = 1080 }
                (\( state, frontendActions ) ->
                    state
                        |> frontendActions.clickButton { htmlId = "myButton" }
                )
            |> Effect.Test.toTest

    unsafeUrl : String -> Url
    unsafeUrl urlText =
        case Url.fromString urlText of
            Just url ->
                url

            Nothing ->
                unsafeUrl urlText

-}
connectFrontend :
    SessionId
    -> Url
    -> { width : Int, height : Int }
    ->
        (( Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
         , FrontendActions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
         )
         -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        )
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
connectFrontend sessionId url windowSize andThenFunc instructions =
    AndThen
        (\state ->
            let
                state2 : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
                state2 =
                    { state
                        | frontends =
                            Dict.insert
                                clientId
                                { model = frontend
                                , sessionId = sessionId
                                , pendingEffects = Array.fromList [ { cmds = cmd, stepIndex = Array.length state.history } ]
                                , toFrontend = []
                                , timers = getTimers subscriptions |> Dict.map (\_ _ -> { startTime = currentTime state })
                                , url = url
                                , windowSize = windowSize
                                }
                                state.frontends
                        , counter = state.counter + 1
                    }
                        |> addEvent (FrontendInitEvent clientId cmd)

                clientId : ClientId
                clientId =
                    "clientId " ++ String.fromInt state.counter |> Effect.Lamdera.clientIdFromString

                ( frontend, cmd ) =
                    state.frontendApp.init url (Effect.Browser.Navigation.fromInternalKey MockNavigationKey)

                subscriptions : Subscription FrontendOnly frontendMsg
                subscriptions =
                    state.frontendApp.subscriptions frontend
            in
            andThenFunc
                ( getClientConnectSubs (state2.backendApp.subscriptions state2.model)
                    |> List.foldl
                        (\msg state3 ->
                            handleBackendUpdate (currentTime state3) state3.backendApp (msg sessionId clientId) state3
                        )
                        state2
                    |> Start
                    |> shortWait
                , { clientId = clientId
                  , keyDownEvent = keyDownEvent clientId
                  , clickButton = clickButton clientId
                  , inputText = inputText clientId
                  , clickLink = clickLink clientId
                  , checkView = checkView clientId
                  , update = frontendUpdate clientId
                  , snapshotView = snapshotView clientId
                  }
                )
        )
        instructions


type alias Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel =
    { eventType : EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , time : Time.Posix
    , frontends : Dict ClientId (EventFrontend frontendModel)
    , backend : backendModel
    , testErrors : List TestError
    , cachedElmValue : Maybe { diff : ElmValue, noDiff : ElmValue }
    }


type alias EventFrontend frontendModel =
    { model : frontendModel
    , sessionId : SessionId
    , url : Url
    , windowSize : { width : Int, height : Int }
    }


type EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    = UpdateFromFrontendEvent { clientId : ClientId, toBackend : toBackend, cmds : Command BackendOnly toFrontend backendMsg, stepIndex : Maybe Int }
    | UpdateFromBackendEvent { clientId : ClientId, toFrontend : toFrontend, cmds : Command FrontendOnly toBackend frontendMsg, stepIndex : Int }
    | BackendUpdateEvent backendMsg (Command BackendOnly toFrontend backendMsg)
    | FrontendUpdateEvent ClientId frontendMsg (Command FrontendOnly toBackend frontendMsg)
    | BackendInitEvent (Command BackendOnly toFrontend backendMsg)
    | FrontendInitEvent ClientId (Command FrontendOnly toBackend frontendMsg)
    | CheckStateEvent { checkType : CheckType, isSuccessful : Bool }
    | UserInputEvent { clientId : ClientId, inputType : UserInputType, isSuccessful : Bool }
    | TestEvent (Maybe ClientId) String
    | SnapshotEvent { clientId : ClientId, name : String, isSuccessful : Bool }


type UserInputType
    = UserClicksButton HtmlId
    | UserInputsText HtmlId String
    | UserPressesKey HtmlId { keyCode : Int }
    | UserClicksLink { href : String }


type CheckType
    = CheckFrontendView ClientId
    | CheckFrontendState ClientId
    | CheckState
    | CheckBackend


handleFrontendUpdate :
    ClientId
    -> Time.Posix
    -> frontendMsg
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
handleFrontendUpdate clientId currentTime2 msg state =
    case Dict.get clientId state.frontends of
        Just frontend ->
            let
                ( newModel, cmd ) =
                    state.frontendApp.update msg frontend.model

                subscriptions : Subscription FrontendOnly frontendMsg
                subscriptions =
                    state.frontendApp.subscriptions newModel

                newTimers : Dict Duration { msg : Nonempty (Time.Posix -> frontendMsg) }
                newTimers =
                    getTimers subscriptions
            in
            { state
                | frontends =
                    Dict.insert
                        clientId
                        { frontend
                            | model = newModel
                            , pendingEffects =
                                Array.push
                                    { cmds = cmd, stepIndex = Array.length state.history }
                                    frontend.pendingEffects
                            , timers =
                                Dict.merge
                                    (\duration _ dict -> Dict.insert duration { startTime = currentTime2 } dict)
                                    (\_ _ _ dict -> dict)
                                    (\duration _ dict -> Dict.remove duration dict)
                                    newTimers
                                    frontend.timers
                                    frontend.timers
                        }
                        state.frontends
            }
                |> addEvent (FrontendUpdateEvent clientId msg cmd)

        Nothing ->
            state


handleBackendUpdate :
    Time.Posix
    -> BackendApp toBackend toFrontend backendMsg backendModel
    -> backendMsg
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
handleBackendUpdate currentTime2 app msg state =
    let
        ( newModel, cmd ) =
            app.update msg state.model

        subscriptions : Subscription BackendOnly backendMsg
        subscriptions =
            app.subscriptions newModel

        newTimers : Dict Duration { msg : Nonempty (Time.Posix -> backendMsg) }
        newTimers =
            getTimers subscriptions
    in
    { state
        | model = newModel
        , pendingEffects =
            Array.push
                { cmds = cmd, stepIndex = Array.length state.history }
                state.pendingEffects
        , timers =
            Dict.merge
                (\duration _ dict -> Dict.insert duration { startTime = currentTime2 } dict)
                (\_ _ _ dict -> dict)
                (\duration _ dict -> Dict.remove duration dict)
                newTimers
                state.timers
                state.timers
    }
        |> addEvent (BackendUpdateEvent msg cmd)


type alias ToFrontendData toFrontend =
    { toFrontend : toFrontend, stepIndex : Int }


handleUpdateFromBackend :
    ClientId
    -> Time.Posix
    -> ToFrontendData toFrontend
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
handleUpdateFromBackend clientId currentTime2 { toFrontend, stepIndex } state =
    case Dict.get clientId state.frontends of
        Just frontendState ->
            let
                ( newModel, cmd ) =
                    state.frontendApp.updateFromBackend toFrontend frontendState.model

                subscriptions : Subscription FrontendOnly frontendMsg
                subscriptions =
                    state.frontendApp.subscriptions newModel

                newTimers : Dict Duration { msg : Nonempty (Time.Posix -> frontendMsg) }
                newTimers =
                    getTimers subscriptions
            in
            { state
                | frontends =
                    Dict.insert
                        clientId
                        { frontendState
                            | model = newModel
                            , pendingEffects =
                                Array.push
                                    { cmds = cmd, stepIndex = Array.length state.history }
                                    frontendState.pendingEffects
                            , timers =
                                Dict.merge
                                    (\duration _ dict -> Dict.insert duration { startTime = currentTime2 } dict)
                                    (\_ _ _ dict -> dict)
                                    (\duration _ dict -> Dict.remove duration dict)
                                    newTimers
                                    frontendState.timers
                                    frontendState.timers
                        }
                        state.frontends
            }
                |> addEvent
                    (UpdateFromBackendEvent
                        { clientId = clientId
                        , toFrontend = toFrontend
                        , cmds = cmd
                        , stepIndex = stepIndex
                        }
                    )

        Nothing ->
            state


handleUpdateFromFrontend :
    ToBackendData toBackend
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
handleUpdateFromFrontend { sessionId, clientId, toBackend, stepIndex } state =
    let
        ( newModel, cmd ) =
            state.backendApp.updateFromFrontend sessionId clientId toBackend state.model

        subscriptions : Subscription BackendOnly backendMsg
        subscriptions =
            state.backendApp.subscriptions newModel

        newTimers : Dict Duration { msg : Nonempty (Time.Posix -> backendMsg) }
        newTimers =
            getTimers subscriptions
    in
    { state
        | model = newModel
        , pendingEffects =
            Array.push
                { cmds = cmd, stepIndex = Array.length state.history }
                state.pendingEffects
        , timers =
            Dict.merge
                (\duration _ dict -> Dict.insert duration { startTime = currentTime state } dict)
                (\_ _ _ dict -> dict)
                (\duration _ dict -> Dict.remove duration dict)
                newTimers
                state.timers
                state.timers
    }
        |> addEvent (UpdateFromFrontendEvent { clientId = clientId, toBackend = toBackend, cmds = cmd, stepIndex = stepIndex })


addEvent :
    EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
addEvent eventType state =
    { state
        | history =
            Array.push
                { eventType = eventType
                , time = currentTime state
                , frontends =
                    Dict.map
                        (\_ a ->
                            { model = a.model
                            , sessionId = a.sessionId
                            , url = a.url
                            , windowSize = a.windowSize
                            }
                        )
                        state.frontends
                , backend = state.model
                , testErrors = state.testErrors
                , cachedElmValue = Nothing
                }
                state.history
    }


snapshotView :
    ClientId
    -> { name : String }
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
snapshotView clientId data instructions =
    NextStep
        (\state ->
            case Dict.get clientId state.frontends of
                Just frontend ->
                    { state
                        | snapshots =
                            { name = data.name
                            , body = state.frontendApp.view frontend.model |> .body
                            , width = frontend.windowSize.width
                            , height = frontend.windowSize.height
                            }
                                :: state.snapshots
                    }
                        |> addEvent (SnapshotEvent { clientId = clientId, name = data.name, isSuccessful = True })

                Nothing ->
                    addTestError (ClientIdNotFound clientId) state
                        |> addEvent (SnapshotEvent { clientId = clientId, name = data.name, isSuccessful = False })
        )
        instructions


{-| -}
keyDownEvent :
    ClientId
    -> HtmlId
    -> { keyCode : Int }
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
keyDownEvent clientId htmlId keyCode =
    userEvent
        (UserPressesKey htmlId keyCode)
        clientId
        htmlId
        ( "keydown", Json.Encode.object [ ( "keyCode", Json.Encode.int keyCode.keyCode ) ] )


{-| -}
clickButton :
    ClientId
    -> HtmlId
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
clickButton clientId htmlId =
    userEvent (UserClicksButton htmlId) clientId htmlId Test.Html.Event.click


{-| -}
inputText :
    ClientId
    -> HtmlId
    -> String
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
inputText clientId htmlId text_ =
    userEvent (UserInputsText htmlId text_) clientId htmlId (Test.Html.Event.input text_)


normalizeUrl : Url -> String -> String
normalizeUrl domainUrl path =
    if String.startsWith "/" path then
        let
            domain =
                Url.toString domainUrl
        in
        if String.endsWith "/" domain then
            String.dropRight 1 domain ++ path

        else
            domain ++ path

    else
        path


{-| -}
clickLink :
    ClientId
    -> { href : String }
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
clickLink clientId data instructions =
    let
        event isSuccessful =
            UserInputEvent { clientId = clientId, inputType = UserClicksLink data, isSuccessful = isSuccessful }
    in
    NextStep
        (\state ->
            case Dict.get clientId state.frontends of
                Just frontend ->
                    case
                        state.frontendApp.view frontend.model
                            |> .body
                            |> Html.div []
                            |> Test.Html.Query.fromHtml
                            |> Test.Html.Query.findAll [ Test.Html.Selector.attribute (Html.Attributes.href data.href) ]
                            |> Test.Html.Query.count
                                (\count ->
                                    if count > 0 then
                                        Expect.pass

                                    else
                                        Expect.fail ("Expected at least one link pointing to " ++ data.href)
                                )
                            |> Test.Runner.getFailureReason
                    of
                        Nothing ->
                            case Url.fromString (normalizeUrl state.domain data.href) of
                                Just url ->
                                    handleFrontendUpdate
                                        clientId
                                        (currentTime state)
                                        (state.frontendApp.onUrlRequest (Internal url))
                                        (addEvent (event True) state)

                                Nothing ->
                                    addTestError (InvalidUrl data.href) state |> addEvent (event False)

                        Just _ ->
                            addTestError
                                (CustomError ("Clicking link failed for " ++ data.href))
                                state
                                |> addEvent (event False)

                Nothing ->
                    addTestError (ClientIdNotFound clientId) state |> addEvent (event False)
        )
        instructions
        |> shortWait


shortWait :
    Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
shortWait instructions =
    simulateTime (Duration.milliseconds 100) instructions


userEvent :
    UserInputType
    -> ClientId
    -> HtmlId
    -> ( String, Json.Encode.Value )
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
userEvent userInputType clientId htmlId event instructions =
    let
        htmlIdString : String
        htmlIdString =
            Effect.Browser.Dom.idToString htmlId

        eventType : Bool -> EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        eventType isSuccessful =
            UserInputEvent { clientId = clientId, inputType = userInputType, isSuccessful = isSuccessful }
    in
    NextStep
        (\state ->
            case Dict.get clientId state.frontends of
                Just frontend ->
                    let
                        query =
                            state.frontendApp.view frontend.model
                                |> .body
                                |> Html.div []
                                |> Test.Html.Query.fromHtml
                                |> Test.Html.Query.find [ Test.Html.Selector.id htmlIdString ]
                    in
                    case Test.Html.Event.simulate event query |> Test.Html.Event.toResult of
                        Ok msg ->
                            handleFrontendUpdate clientId (currentTime state) msg (addEvent (eventType True) state)

                        Err _ ->
                            case Test.Runner.getFailureReason (Test.Html.Query.has [] query) of
                                Just { description } ->
                                    addTestError
                                        (CustomError ("User event failed for element with id " ++ htmlIdString))
                                        state
                                        |> addEvent (eventType False)

                                Nothing ->
                                    addTestError
                                        (CustomError ("Unable to find element with id " ++ htmlIdString))
                                        state
                                        |> addEvent (eventType False)

                Nothing ->
                    addTestError (ClientIdNotFound clientId) state |> addEvent (eventType False)
        )
        instructions
        |> shortWait


{-| -}
disconnectFrontend :
    BackendApp toBackend toFrontend backendMsg backendModel
    -> ClientId
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> ( State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel, Maybe (FrontendState toBackend frontendMsg frontendModel toFrontend) )
disconnectFrontend backendApp clientId state =
    case Dict.get clientId state.frontends of
        Just frontend ->
            let
                state2 : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
                state2 =
                    getClientDisconnectSubs (backendApp.subscriptions state.model)
                        |> List.foldl
                            (\msg state3 ->
                                handleBackendUpdate (currentTime state3) state3.backendApp (msg frontend.sessionId clientId) state3
                            )
                            state
            in
            ( { state2 | frontends = Dict.remove clientId state2.frontends }
            , Just { frontend | toFrontend = [] }
            )

        Nothing ->
            ( state, Nothing )


{-| Normally you won't send data directly to the backend and instead use `connectFrontend` followed by things like `clickButton` or `inputText` to cause the frontend to send data to the backend.
If you do need to send data directly, then you can use this though.
-}
sendToBackend :
    SessionId
    -> ClientId
    -> toBackend
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
sendToBackend sessionId clientId toBackend =
    NextStep
        (\state ->
            { state
                | toBackend =
                    state.toBackend
                        ++ [ { sessionId = sessionId, clientId = clientId, toBackend = toBackend, stepIndex = Nothing } ]
            }
                |> addEvent (TestEvent (Just clientId) ("Trigger ToBackend: " ++ Debug.toString toBackend))
        )


animationFrame : Duration
animationFrame =
    Duration.seconds (1 / 60)


timerEndTimes : Dict Duration { startTime : Time.Posix } -> List { endTime : Time.Posix, duration : Duration }
timerEndTimes dict =
    List.map
        (\( duration, a ) -> { endTime = Duration.addTo a.startTime duration, duration = duration })
        (Dict.toList dict)


{-| Find the first minimum element in a list using a comparable transformation. Copied from elm-community/list-extra package
-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
    let
        minBy x ( y, fy ) =
            let
                fx =
                    f x
            in
            if fx < fy then
                ( x, fx )

            else
                ( y, fy )
    in
    case ls of
        [ l_ ] ->
            Just l_

        l_ :: ls_ ->
            Just <| Tuple.first <| List.foldl minBy ( l_, f l_ ) ls_

        _ ->
            Nothing


currentTime : { a | elapsedTime : Duration } -> Time.Posix
currentTime state =
    Time.posixToMillis startTime + elapsedTimeInMillis state |> Time.millisToPosix


elapsedTimeInMillis : { a | elapsedTime : Duration } -> Int
elapsedTimeInMillis state =
    Duration.inMilliseconds state.elapsedTime |> round


getTriggersTimerMsgs :
    (model -> Subscription restriction msg)
    -> { c | timers : Dict Duration { startTime : Time.Posix }, model : model }
    -> Time.Posix
    -> { triggeredMsgs : List msg, completedDurations : List Duration }
getTriggersTimerMsgs subscriptionsFunc state endTime =
    let
        completedDurations : List Duration
        completedDurations =
            List.filterMap
                (\b ->
                    if Time.posixToMillis b.endTime <= Time.posixToMillis endTime then
                        Just b.duration

                    else
                        Nothing
                )
                (timerEndTimes state.timers)
    in
    { triggeredMsgs =
        subscriptionsFunc state.model
            |> getTimers
            |> Dict.filter (\duration _ -> List.member duration completedDurations)
            |> Dict.values
            |> List.concatMap (\value -> List.Nonempty.toList value.msg)
            |> List.map (\msg -> msg endTime)
    , completedDurations = completedDurations
    }


hasPendingEffects : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> Bool
hasPendingEffects state =
    let
        hasEffectsHelper pendingEffects =
            Array.foldl
                (\{ cmds } hasEffects -> hasEffects || not (List.isEmpty (flattenEffects cmds)))
                False
                pendingEffects
    in
    hasEffectsHelper state.pendingEffects
        || List.any (\a -> hasEffectsHelper a.pendingEffects) (Dict.values state.frontends)


simulateStep :
    Duration
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
simulateStep timeLeft state =
    case
        timerEndTimes state.timers
            ++ List.concatMap (\( _, frontend ) -> timerEndTimes frontend.timers) (Dict.toList state.frontends)
            |> minimumBy (\a -> Time.posixToMillis a.endTime)
    of
        Just nextTimerEnd ->
            let
                delta : Duration
                delta =
                    Duration.from (currentTime state) nextTimerEnd.endTime
            in
            if
                hasPendingEffects state
                    && (timeLeft |> Quantity.greaterThanOrEqualTo animationFrame)
                    && (delta |> Quantity.greaterThan animationFrame)
            then
                runEffects { state | elapsedTime = Quantity.plus animationFrame state.elapsedTime }
                    |> simulateStep (timeLeft |> Quantity.minus animationFrame)

            else if delta |> Quantity.lessThanOrEqualTo timeLeft then
                let
                    state2 =
                        let
                            { triggeredMsgs, completedDurations } =
                                getTriggersTimerMsgs state.backendApp.subscriptions state nextTimerEnd.endTime
                        in
                        List.foldl
                            (handleBackendUpdate nextTimerEnd.endTime state.backendApp)
                            { state | timers = List.foldl Dict.remove state.timers completedDurations }
                            triggeredMsgs

                    state3 =
                        Dict.foldl
                            (\clientId frontend state4 ->
                                let
                                    { triggeredMsgs, completedDurations } =
                                        getTriggersTimerMsgs state2.frontendApp.subscriptions frontend nextTimerEnd.endTime
                                in
                                List.foldl
                                    (handleFrontendUpdate clientId nextTimerEnd.endTime)
                                    { state4
                                        | frontends =
                                            Dict.insert
                                                clientId
                                                { frontend | timers = List.foldl Dict.remove frontend.timers completedDurations }
                                                state4.frontends
                                    }
                                    triggeredMsgs
                            )
                            state2
                            state2.frontends
                in
                simulateStep
                    (timeLeft |> Quantity.minus delta)
                    { state3
                        | elapsedTime = Duration.from startTime nextTimerEnd.endTime
                    }
                    |> runEffects

            else
                { state | elapsedTime = Quantity.plus state.elapsedTime timeLeft }

        Nothing ->
            if hasPendingEffects state && (timeLeft |> Quantity.greaterThanOrEqualTo animationFrame) then
                runEffects { state | elapsedTime = Quantity.plus animationFrame state.elapsedTime }
                    |> simulateStep (timeLeft |> Quantity.minus animationFrame)

            else
                { state | elapsedTime = Quantity.plus state.elapsedTime timeLeft }


{-| Simulate the passage of time.
This will trigger any subscriptions like `Browser.onAnimationFrame` or `Time.every` along the way.

If you need to simulate a large passage of time and are finding that it's taking too long to run, try `fastForward` instead.

-}
simulateTime :
    Duration
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
simulateTime duration =
    NextStep (simulateStep duration)


{-| Similar to `simulateTime` but this will not trigger any `Browser.onAnimationFrame` or `Time.every` subscriptions.

This is useful if you need to move the clock forward a week and it would take too long to simulate it perfectly.

-}
fastForward :
    Duration
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
fastForward duration =
    NextStep
        (\state ->
            addEvent
                (TestEvent
                    Nothing
                    ("Fast forward " ++ String.fromFloat (Duration.inSeconds duration) ++ "s (skip timer events)")
                )
                { state | elapsedTime = Quantity.plus state.elapsedTime duration }
        )


{-| Sometimes you need to decide what should happen next based on some current state.
In order to do that you can write something like this:

    import Effect.Test

    state
        |> Effect.Test.andThen
            (\state2 ->
                case List.filterMap isLoginEmail state2.httpRequests |> List.head of
                    Just loginEmail ->
                        Effect.Test.continueWith state2
                                |> testApp.connectFrontend
                                    sessionIdFromEmail
                                    (loginEmail.loginUrl)
                                    (\( state3, clientIdFromEmail ) ->
                                        ...
                                    )

                    Nothing ->
                        Effect.Test.continueWith state2 |> Effect.Test.checkState (\_ -> Err "Should have gotten a login email")
            )

-}
andThen :
    (Data frontendModel backendModel -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
andThen andThenFunc instructions =
    AndThen (\state -> andThenFunc (stateToData state) (Start state)) instructions


runEffects :
    State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
runEffects state =
    let
        state2 : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        state2 =
            Array.foldl (\a state6 -> runBackendEffects a.stepIndex a.cmds state6) (clearBackendEffects state) state.pendingEffects
    in
    Dict.foldl
        (\clientId { sessionId, pendingEffects } state3 ->
            Array.foldl
                (\a state6 -> runFrontendEffects sessionId clientId a.stepIndex a.cmds state6)
                (clearFrontendEffects clientId state3)
                pendingEffects
        )
        state2
        state2.frontends
        |> runNetwork


runNetwork :
    State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
runNetwork state =
    let
        state2 : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        state2 =
            List.foldl handleUpdateFromFrontend state state.toBackend
    in
    Dict.foldl
        (\clientId frontend state4 ->
            List.foldl
                (handleUpdateFromBackend clientId (currentTime state4))
                { state4 | frontends = Dict.insert clientId { frontend | toFrontend = [] } state4.frontends }
                frontend.toFrontend
        )
        { state2 | toBackend = [] }
        state2.frontends


clearBackendEffects :
    State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
clearBackendEffects state =
    { state | pendingEffects = Array.empty }


clearFrontendEffects :
    ClientId
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
clearFrontendEffects clientId state =
    { state
        | frontends =
            Dict.update
                clientId
                (Maybe.map (\frontend -> { frontend | pendingEffects = Array.empty }))
                state.frontends
    }


runFrontendEffects :
    SessionId
    -> ClientId
    -> Int
    -> Command FrontendOnly toBackend frontendMsg
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
runFrontendEffects sessionId clientId stepIndex effectsToPerform state =
    case effectsToPerform of
        Batch nestedEffectsToPerform ->
            List.foldl (runFrontendEffects sessionId clientId stepIndex) state nestedEffectsToPerform

        SendToBackend toBackend ->
            { state
                | toBackend =
                    state.toBackend
                        ++ [ { sessionId = sessionId
                             , clientId = clientId
                             , toBackend = toBackend
                             , stepIndex = Just stepIndex
                             }
                           ]
            }

        NavigationPushUrl _ urlText ->
            handleUrlChange urlText clientId state

        NavigationReplaceUrl _ urlText ->
            handleUrlChange urlText clientId state

        NavigationLoad urlText ->
            handleUrlChange urlText clientId state

        NavigationBack _ _ ->
            -- TODO
            state

        NavigationForward _ _ ->
            -- TODO
            state

        NavigationReload ->
            -- TODO
            state

        NavigationReloadAndSkipCache ->
            -- TODO
            state

        None ->
            state

        Task task ->
            let
                ( newState, msg ) =
                    runTask (Just clientId) state task
            in
            handleFrontendUpdate clientId (currentTime newState) msg newState

        Port portName _ value ->
            let
                portRequest =
                    { clientId = clientId, portName = portName, value = value }

                newState =
                    { state | portRequests = portRequest :: state.portRequests }
            in
            case
                newState.handlePortToJs
                    { currentRequest = portRequest
                    , data = stateToData state
                    }
            of
                Just ( responsePortName, responseValue ) ->
                    case Dict.get clientId state.frontends of
                        Just frontend ->
                            let
                                msgs : List frontendMsg
                                msgs =
                                    state.frontendApp.subscriptions frontend.model
                                        |> getPortSubscriptions
                                        |> List.filterMap
                                            (\sub ->
                                                if sub.portName == responsePortName then
                                                    Just (sub.msg responseValue)

                                                else
                                                    Nothing
                                            )
                            in
                            List.foldl (handleFrontendUpdate clientId (currentTime state)) newState msgs

                        Nothing ->
                            newState

                Nothing ->
                    newState

        SendToFrontend _ _ ->
            state

        SendToFrontends _ _ ->
            state

        FileDownloadUrl _ ->
            state

        FileDownloadString data ->
            { state
                | downloads =
                    { filename = data.name
                    , mimeType = data.mimeType
                    , content = StringFile data.content
                    , downloadedAt = currentTime state
                    }
                        :: state.downloads
            }

        FileDownloadBytes data ->
            { state
                | downloads =
                    { filename = data.name
                    , mimeType = data.mimeType
                    , content = BytesFile data.content
                    , downloadedAt = currentTime state
                    }
                        :: state.downloads
            }

        FileSelectFile mimeTypes msg ->
            case state.handleFileUpload { mimeTypes = mimeTypes, data = stateToData state } of
                UploadFile (FileUploadData file) ->
                    handleFrontendUpdate clientId (currentTime state) (msg (Effect.Internal.MockFile file)) state

                CancelFileUpload ->
                    state

                UnhandledFileUpload ->
                    addTestError FileUploadNotHandled state

        FileSelectFiles mimeTypes msg ->
            case state.handleMultipleFilesUpload { mimeTypes = mimeTypes, data = stateToData state } of
                UploadMultipleFiles (FileUploadData file) files ->
                    handleFrontendUpdate
                        clientId
                        (currentTime state)
                        (msg
                            (Effect.Internal.MockFile file)
                            (List.map (\(FileUploadData a) -> Effect.Internal.MockFile a) files)
                        )
                        state

                CancelMultipleFilesUpload ->
                    state

                UnhandledMultiFileUpload ->
                    addTestError MultipleFilesUploadNotHandled state

        Broadcast _ ->
            state

        HttpCancel _ ->
            -- TODO
            state

        Passthrough _ ->
            state


getPortSubscriptions :
    Subscription FrontendOnly frontendMsg
    -> List { portName : String, msg : Json.Decode.Value -> frontendMsg }
getPortSubscriptions subscription =
    case subscription of
        Effect.Internal.SubBatch subscriptions ->
            List.concatMap getPortSubscriptions subscriptions

        Effect.Internal.SubPort portName _ msg ->
            [ { portName = portName, msg = msg } ]

        _ ->
            []


handleUrlChange :
    String
    -> ClientId
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
handleUrlChange urlText clientId state =
    case normalizeUrl state.domain urlText |> Url.fromString of
        Just url ->
            let
                state2 : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
                state2 =
                    handleFrontendUpdate clientId (currentTime state) (state.frontendApp.onUrlChange url) state
            in
            { state2
                | frontends =
                    Dict.update clientId (Maybe.map (\newFrontend -> { newFrontend | url = url })) state2.frontends
            }

        Nothing ->
            state


flattenEffects : Command restriction toBackend frontendMsg -> List (Command restriction toBackend frontendMsg)
flattenEffects effect =
    case effect of
        Batch effects ->
            List.concatMap flattenEffects effects

        None ->
            []

        _ ->
            [ effect ]


runBackendEffects :
    Int
    -> Command BackendOnly toFrontend backendMsg
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
runBackendEffects stepIndex effect state =
    case effect of
        Batch effects ->
            List.foldl (runBackendEffects stepIndex) state effects

        SendToFrontend (Effect.Internal.ClientId clientId) toFrontend ->
            { state
                | frontends =
                    Dict.update
                        (Effect.Lamdera.clientIdFromString clientId)
                        (Maybe.map
                            (\frontend ->
                                { frontend
                                    | toFrontend =
                                        frontend.toFrontend
                                            ++ [ { toFrontend = toFrontend, stepIndex = stepIndex } ]
                                }
                            )
                        )
                        state.frontends
            }

        SendToFrontends (Effect.Internal.SessionId sessionId) toFrontend ->
            let
                sessionId_ =
                    Effect.Lamdera.sessionIdFromString sessionId
            in
            { state
                | frontends =
                    Dict.map
                        (\_ frontend ->
                            if frontend.sessionId == sessionId_ then
                                { frontend
                                    | toFrontend =
                                        frontend.toFrontend
                                            ++ [ { toFrontend = toFrontend, stepIndex = stepIndex } ]
                                }

                            else
                                frontend
                        )
                        state.frontends
            }

        None ->
            state

        Task task ->
            let
                ( state2, msg ) =
                    runTask Nothing state task
            in
            handleBackendUpdate (currentTime state2) state2.backendApp msg state2

        SendToBackend _ ->
            state

        NavigationPushUrl _ _ ->
            state

        NavigationReplaceUrl _ _ ->
            state

        NavigationLoad _ ->
            state

        NavigationBack _ _ ->
            state

        NavigationForward _ _ ->
            state

        NavigationReload ->
            state

        NavigationReloadAndSkipCache ->
            state

        Port _ _ _ ->
            state

        FileDownloadUrl _ ->
            state

        FileDownloadString _ ->
            state

        FileDownloadBytes _ ->
            state

        FileSelectFile _ _ ->
            state

        FileSelectFiles _ _ ->
            state

        Broadcast toFrontend ->
            { state
                | frontends =
                    Dict.map
                        (\_ frontend -> { frontend | toFrontend = frontend.toFrontend ++ [ { toFrontend = toFrontend, stepIndex = stepIndex } ] })
                        state.frontends
            }

        HttpCancel _ ->
            -- TODO
            state

        Passthrough _ ->
            state


runTask :
    Maybe ClientId
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Task restriction x x
    -> ( State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel, x )
runTask maybeClientId state task =
    case task of
        Succeed value ->
            ( state, value )

        Fail value ->
            ( state, value )

        HttpStringTask httpRequest ->
            -- TODO: Implement actual delays to http requests
            let
                request : HttpRequest
                request =
                    { requestedBy =
                        case maybeClientId of
                            Just clientId ->
                                RequestedByFrontend clientId

                            Nothing ->
                                RequestedByBackend
                    , method = httpRequest.method
                    , url = httpRequest.url
                    , body = httpBodyFromInternal httpRequest.body
                    , headers = httpRequest.headers
                    , sentAt = currentTime state
                    }

                handleResponse : Http.Response String -> ( State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel, x )
                handleResponse a =
                    httpRequest.onRequestComplete a
                        |> runTask maybeClientId { state | httpRequests = request :: state.httpRequests }
            in
            case state.handleHttpRequest { currentRequest = request, data = stateToData state } of
                BadUrlResponse url ->
                    Http.BadUrl_ url |> handleResponse

                TimeoutResponse ->
                    handleResponse Http.Timeout_

                NetworkErrorResponse ->
                    handleResponse Http.NetworkError_

                BadStatusResponse metadata text2 ->
                    Http.BadStatus_ metadata text2 |> handleResponse

                BytesHttpResponse metadata body ->
                    case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width body)) body of
                        Just text2 ->
                            Http.GoodStatus_ metadata text2 |> handleResponse

                        Nothing ->
                            handleHttpResponseWithTestError maybeClientId request httpRequest HttpResponseContainsBytesThatCantConvertToString state

                StringHttpResponse metadata text2 ->
                    Http.GoodStatus_ metadata text2 |> handleResponse

                JsonHttpResponse metadata body ->
                    Http.GoodStatus_ metadata (Json.Encode.encode 0 body) |> handleResponse

                TextureHttpResponse _ _ ->
                    handleHttpResponseWithTestError maybeClientId request httpRequest HttpResponseCantConvertTextureToString state

                UnhandledHttpRequest ->
                    handleHttpResponseWithTestError maybeClientId request httpRequest HttpRequestNotHandled state

        HttpBytesTask httpRequest ->
            -- TODO: Implement actual delays to http requests
            let
                request : HttpRequest
                request =
                    { requestedBy =
                        case maybeClientId of
                            Just clientId ->
                                RequestedByFrontend clientId

                            Nothing ->
                                RequestedByBackend
                    , method = httpRequest.method
                    , url = httpRequest.url
                    , body = httpBodyFromInternal httpRequest.body
                    , headers = httpRequest.headers
                    , sentAt = currentTime state
                    }

                handleResponse a =
                    httpRequest.onRequestComplete a
                        |> runTask maybeClientId { state | httpRequests = request :: state.httpRequests }
            in
            case state.handleHttpRequest { currentRequest = request, data = stateToData state } of
                BadUrlResponse url ->
                    Http.BadUrl_ url |> handleResponse

                TimeoutResponse ->
                    handleResponse Http.Timeout_

                NetworkErrorResponse ->
                    handleResponse Http.NetworkError_

                BadStatusResponse metadata text2 ->
                    Http.BadStatus_ metadata (Bytes.Encode.string text2 |> Bytes.Encode.encode) |> handleResponse

                BytesHttpResponse metadata body ->
                    Http.GoodStatus_ metadata body |> handleResponse

                StringHttpResponse metadata text2 ->
                    Http.GoodStatus_ metadata (Bytes.Encode.string text2 |> Bytes.Encode.encode) |> handleResponse

                JsonHttpResponse metadata body ->
                    Http.GoodStatus_
                        metadata
                        (Json.Encode.encode 0 body |> Bytes.Encode.string |> Bytes.Encode.encode)
                        |> handleResponse

                TextureHttpResponse _ _ ->
                    handleHttpResponseWithTestError maybeClientId request httpRequest HttpResponseCantConvertTextureToString state

                UnhandledHttpRequest ->
                    handleHttpResponseWithTestError maybeClientId request httpRequest HttpRequestNotHandled state

        SleepTask _ function ->
            -- TODO: Implement actual delays in tasks
            runTask maybeClientId state (function ())

        TimeNow gotTime ->
            gotTime (currentTime state) |> runTask maybeClientId state

        TimeHere gotTimeZone ->
            gotTimeZone Time.utc |> runTask maybeClientId state

        TimeGetZoneName getTimeZoneName ->
            getTimeZoneName (Time.Offset 0) |> runTask maybeClientId state

        GetViewport function ->
            (case maybeClientId of
                Just clientId ->
                    case Dict.get clientId state.frontends of
                        Just frontend ->
                            function
                                { scene =
                                    { width = toFloat frontend.windowSize.width
                                    , height = toFloat frontend.windowSize.height
                                    }
                                , viewport =
                                    { x = 0
                                    , y = 0
                                    , width = toFloat frontend.windowSize.width
                                    , height = toFloat frontend.windowSize.height
                                    }
                                }

                        Nothing ->
                            function { scene = { width = 1920, height = 1080 }, viewport = { x = 0, y = 0, width = 1920, height = 1080 } }

                Nothing ->
                    function { scene = { width = 1920, height = 1080 }, viewport = { x = 0, y = 0, width = 1920, height = 1080 } }
            )
                |> runTask maybeClientId state

        SetViewport _ _ function ->
            function () |> runTask maybeClientId state

        GetElement htmlId function ->
            getDomTask
                maybeClientId
                state
                htmlId
                function
                { scene = { width = 100, height = 100 }
                , viewport = { x = 0, y = 0, width = 100, height = 100 }
                , element = { x = 0, y = 0, width = 100, height = 100 }
                }

        FileToString file function ->
            case file of
                Effect.Internal.RealFile _ ->
                    function "" |> runTask maybeClientId state

                Effect.Internal.MockFile { content } ->
                    (case content of
                        Effect.Internal.StringFile a ->
                            a

                        Effect.Internal.BytesFile a ->
                            Bytes.Decode.decode (Bytes.Decode.string (Bytes.width a)) a
                                |> Maybe.withDefault ""
                    )
                        |> function
                        |> runTask maybeClientId state

        FileToBytes file function ->
            case file of
                Effect.Internal.RealFile _ ->
                    function (Bytes.Encode.encode (Bytes.Encode.sequence []))
                        |> runTask maybeClientId state

                Effect.Internal.MockFile { content } ->
                    (case content of
                        Effect.Internal.StringFile a ->
                            Bytes.Encode.encode (Bytes.Encode.string a)

                        Effect.Internal.BytesFile a ->
                            a
                    )
                        |> function
                        |> runTask maybeClientId state

        FileToUrl file function ->
            case file of
                Effect.Internal.RealFile _ ->
                    function "" |> runTask maybeClientId state

                Effect.Internal.MockFile { content } ->
                    (case content of
                        Effect.Internal.StringFile a ->
                            "data:*/*;base64," ++ Maybe.withDefault "" (Base64.fromString a)

                        Effect.Internal.BytesFile a ->
                            "data:*/*;base64," ++ Maybe.withDefault "" (Base64.fromBytes a)
                    )
                        |> function
                        |> runTask maybeClientId state

        Focus htmlId function ->
            getDomTask maybeClientId state htmlId function ()

        Blur htmlId function ->
            getDomTask maybeClientId state htmlId function ()

        GetViewportOf htmlId function ->
            getDomTask
                maybeClientId
                state
                htmlId
                function
                { scene = { width = 100, height = 100 }
                , viewport = { x = 0, y = 0, width = 100, height = 100 }
                }

        SetViewportOf htmlId _ _ function ->
            getDomTask maybeClientId state htmlId function ()

        LoadTexture _ url function ->
            let
                response : HttpResponse
                response =
                    state.handleHttpRequest
                        { currentRequest =
                            { requestedBy =
                                case maybeClientId of
                                    Just clientId ->
                                        RequestedByFrontend clientId

                                    Nothing ->
                                        RequestedByBackend
                            , method = "GET"
                            , url = url
                            , body = EmptyBody
                            , headers = []
                            , sentAt = currentTime state
                            }
                        , data = stateToData state
                        }
            in
            (case response of
                TextureHttpResponse _ texture ->
                    Ok texture

                _ ->
                    Err WebGLFix.Texture.LoadError
            )
                |> function
                |> runTask maybeClientId state

        RequestXrStart _ function ->
            function (Err Effect.Internal.NotSupported) |> runTask maybeClientId state

        RenderXrFrame _ function ->
            function
                (Ok
                    { transform = Mat4.identity
                    , views = []
                    , time =
                        currentTime state
                            |> Time.posixToMillis
                            |> toFloat
                    }
                )
                |> runTask maybeClientId state


handleHttpResponseWithTestError :
    Maybe ClientId
    -> HttpRequest
    -> Effect.Internal.HttpRequest a restriction x x
    -> (HttpRequest -> TestError)
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> ( State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel, x )
handleHttpResponseWithTestError maybeClientId request httpRequest error state =
    runTask
        maybeClientId
        (addTestError (error request) { state | httpRequests = request :: state.httpRequests })
        (httpRequest.onRequestComplete Http.NetworkError_)


getDomTask :
    Maybe ClientId
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> String
    -> (Result Effect.Internal.BrowserDomError value -> Task restriction x x)
    -> value
    -> ( State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel, x )
getDomTask maybeClientId state htmlId function value =
    (case Maybe.andThen (\clientId -> Dict.get clientId state.frontends) maybeClientId of
        Just frontend ->
            state.frontendApp.view frontend.model
                |> .body
                |> Html.div []
                |> Test.Html.Query.fromHtml
                |> Test.Html.Query.has [ Test.Html.Selector.id htmlId ]
                |> Test.Runner.getFailureReason
                |> (\a ->
                        if a == Nothing then
                            Effect.Internal.BrowserDomNotFound htmlId |> Err

                        else
                            Ok value
                   )

        Nothing ->
            Effect.Internal.BrowserDomNotFound htmlId |> Err
    )
        |> function
        |> runTask maybeClientId state



-- Viewer


{-| -}
type alias Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel =
    { navigationKey : Browser.Navigation.Key
    , currentTest : Maybe (TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    , testResults : List (Result TestError ())
    , tests : Maybe (Result FileLoadError (List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)))
    , windowSize : ( Int, Int )
    }


type alias FileLoadError =
    { name : String
    , error : FileLoadErrorType
    }


type FileLoadErrorType
    = HttpError Http.Error
    | TextureError WebGLFix.Texture.Error


type alias TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel =
    { index : Int
    , testName : String
    , stepIndex : Int
    , steps : Array (Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    , timelines : Array CurrentTimeline
    , timelineIndex : Int
    , overlayPosition : OverlayPosition
    , showModel : Bool
    , collapsedFields : RegularDict.Dict (List String) CollapsedField
    }


type OverlayPosition
    = Top
    | Bottom


{-| -}
type Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | PressedViewTest Int
    | PressedBackToOverview
    | ShortPauseFinished
    | NoOp
    | GotFilesForTests (Result FileLoadError (List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)))
    | PressedToggleOverlayPosition
    | PressedShowModel
    | PressedHideModel
    | PressedExpandField (List PathNode)
    | PressedCollapseField (List PathNode)
    | PressedArrowKey ArrowKey
    | ChangedEventSlider String
    | GotWindowSize Int Int
    | PressedTimelineEvent Int
    | PressedTimeline CurrentTimeline


init :
    ()
    -> Url
    -> Browser.Navigation.Key
    ->
        ( Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        , Cmd (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        )
init _ _ navigationKey =
    ( { navigationKey = navigationKey
      , currentTest = Nothing
      , testResults = []
      , tests = Nothing
      , windowSize = ( 1920, 1080 )
      }
    , Cmd.batch
        [ Process.sleep 0 |> Task.perform (\() -> ShortPauseFinished)
        , Browser.Dom.getViewport
            |> Task.perform (\{ viewport } -> GotWindowSize (round viewport.width) (round viewport.height))
        ]
    )


update :
    ViewerWith (List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
    -> Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    ->
        ( Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        , Cmd (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        )
update config msg model =
    (case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal _ ->
                    ( model, Cmd.none )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        UrlChanged _ ->
            ( model, Cmd.none )

        PressedViewTest index ->
            case model.tests of
                Just (Err _) ->
                    ( model, Cmd.none )

                Just (Ok tests) ->
                    case getAt index tests of
                        Just test ->
                            let
                                state : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
                                state =
                                    instructionsToState test
                            in
                            ( { model
                                | currentTest =
                                    { index = index
                                    , testName = state.testName
                                    , steps = state.history
                                    , timelines = getTimelines2 state.history
                                    , timelineIndex = 0
                                    , stepIndex = 0
                                    , overlayPosition = Bottom
                                    , showModel = False
                                    , collapsedFields = RegularDict.empty
                                    }
                                        |> Just
                              }
                            , Browser.Dom.setViewportOf timelineContainerId 0 0 |> Task.attempt (\_ -> NoOp)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        PressedBackToOverview ->
            ( { model | currentTest = Nothing }, Cmd.none )

        ShortPauseFinished ->
            ( model, Task.attempt GotFilesForTests config.cmds )

        GotFilesForTests result ->
            case result of
                Ok tests ->
                    case getAt (List.length model.testResults) tests of
                        Just test ->
                            ( { model
                                | testResults =
                                    model.testResults
                                        ++ [ case instructionsToState test |> .testErrors of
                                                firstError :: _ ->
                                                    Err firstError

                                                [] ->
                                                    Ok ()
                                           ]
                                , tests = Just (Ok tests)
                              }
                            , Process.sleep 0 |> Task.perform (\() -> ShortPauseFinished)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Err error ->
                    ( { model | tests = Just (Err error) }, Cmd.none )

        PressedToggleOverlayPosition ->
            updateCurrentTest
                (\currentTest ->
                    ( { currentTest
                        | overlayPosition =
                            case currentTest.overlayPosition of
                                Top ->
                                    Bottom

                                Bottom ->
                                    Top
                      }
                    , Cmd.none
                    )
                )
                model

        PressedShowModel ->
            updateCurrentTest (\currentTest -> ( { currentTest | showModel = True }, Cmd.none )) model

        PressedHideModel ->
            updateCurrentTest (\currentTest -> ( { currentTest | showModel = False }, Cmd.none )) model

        PressedExpandField pathNodes ->
            updateCurrentTest
                (\currentTest ->
                    ( { currentTest
                        | collapsedFields =
                            RegularDict.insert
                                (List.map Effect.TreeView.pathNodeToKey pathNodes)
                                FieldIsExpanded
                                currentTest.collapsedFields
                      }
                    , Cmd.none
                    )
                )
                model

        PressedCollapseField pathNodes ->
            updateCurrentTest
                (\currentTest ->
                    ( { currentTest
                        | collapsedFields =
                            RegularDict.insert
                                (List.map Effect.TreeView.pathNodeToKey pathNodes)
                                FieldIsCollapsed
                                currentTest.collapsedFields
                      }
                    , Cmd.none
                    )
                )
                model

        PressedArrowKey arrowKey ->
            updateCurrentTest
                (\currentTest ->
                    case arrowKey of
                        ArrowRight ->
                            case nextTimelineStep False currentTest.stepIndex (currentTimeline currentTest) currentTest of
                                Just ( nextIndex, _ ) ->
                                    stepTo nextIndex currentTest

                                Nothing ->
                                    ( currentTest, Cmd.none )

                        ArrowLeft ->
                            case previousTimelineStep False currentTest.stepIndex (currentTimeline currentTest) currentTest of
                                Just ( previousIndex, _ ) ->
                                    stepTo previousIndex currentTest

                                Nothing ->
                                    ( currentTest, Cmd.none )

                        ArrowUp ->
                            ( { currentTest | timelineIndex = currentTest.timelineIndex - 1 |> max 0 }, Cmd.none )

                        ArrowDown ->
                            ( { currentTest
                                | timelineIndex =
                                    currentTest.timelineIndex + 1 |> min (Array.length currentTest.timelines - 1)
                              }
                            , Cmd.none
                            )
                )
                model

        ChangedEventSlider a ->
            case String.toInt a of
                Just stepIndex ->
                    updateCurrentTest (stepTo stepIndex) model

                Nothing ->
                    ( model, Cmd.none )

        GotWindowSize width height ->
            ( { model | windowSize = ( width, height ) }, Cmd.none )

        PressedTimelineEvent stepIndex ->
            updateCurrentTest (stepTo stepIndex) model

        PressedTimeline timelineType ->
            updateCurrentTest
                (\currentTest ->
                    case arrayFindIndex timelineType currentTest.timelines of
                        Just timelineIndex ->
                            ( { currentTest | timelineIndex = timelineIndex }, Cmd.none )

                        Nothing ->
                            ( currentTest, Cmd.none )
                )
                model
    )
        |> checkCachedElmValue


type CurrentTimeline
    = BackendTimeline
    | FrontendTimeline ClientId


arrayFindIndex : a -> Array a -> Maybe Int
arrayFindIndex item array =
    Array.foldl
        (\item2 ( index, found ) ->
            if found then
                ( index, found )

            else if item2 == item then
                ( index, True )

            else
                ( index + 1, False )
        )
        ( 0, False )
        array
        |> (\( index, found ) ->
                if found then
                    Just index

                else
                    Nothing
           )


stepTo :
    Int
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    ->
        ( TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        , Cmd (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        )
stepTo stepIndex currentTest =
    case Array.get stepIndex currentTest.steps of
        Just step ->
            let
                newTimeline : CurrentTimeline
                newTimeline =
                    eventTypeToTimelineType step.eventType
            in
            ( { currentTest
                | stepIndex = stepIndex
                , timelineIndex =
                    arrayFindIndex newTimeline currentTest.timelines |> Maybe.withDefault currentTest.timelineIndex
              }
            , Browser.Dom.getElement timelineContainerId
                |> Task.andThen
                    (\container ->
                        Browser.Dom.setViewportOf
                            timelineContainerId
                            (toFloat stepIndex * timelineColumnWidth - container.element.width / 2)
                            0
                    )
                |> Task.attempt (\_ -> NoOp)
            )

        Nothing ->
            ( currentTest, Cmd.none )


checkCachedElmValue :
    ( Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    , Cmd (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    )
    ->
        ( Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        , Cmd (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        )
checkCachedElmValue ( model, cmd ) =
    let
        ( model2, cmd2 ) =
            updateCurrentTest
                (\currentTest ->
                    let
                        currentAndPreviousStep : { previousStep : Maybe Int, currentStep : Maybe Int }
                        currentAndPreviousStep =
                            currentAndPreviousStepIndex currentTest
                    in
                    ( case ( currentTest.showModel, model.tests ) of
                        ( True, Just (Ok tests) ) ->
                            case getAt currentTest.index tests of
                                Just test ->
                                    let
                                        state : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
                                        state =
                                            getState test

                                        steps2 : Array (Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
                                        steps2 =
                                            case currentAndPreviousStep.currentStep of
                                                Just currentIndex ->
                                                    updateAt
                                                        currentIndex
                                                        (\event ->
                                                            case event.cachedElmValue of
                                                                Just _ ->
                                                                    event

                                                                Nothing ->
                                                                    checkCachedElmValueHelper event state
                                                        )
                                                        currentTest.steps

                                                Nothing ->
                                                    currentTest.steps
                                    in
                                    { currentTest
                                        | steps =
                                            case currentAndPreviousStep.previousStep of
                                                Just previousIndex ->
                                                    updateAt
                                                        previousIndex
                                                        (\event ->
                                                            case event.cachedElmValue of
                                                                Just _ ->
                                                                    event

                                                                Nothing ->
                                                                    checkCachedElmValueHelper event state
                                                        )
                                                        steps2

                                                Nothing ->
                                                    steps2
                                    }

                                Nothing ->
                                    currentTest

                        _ ->
                            currentTest
                    , Cmd.none
                    )
                )
                model
    in
    ( model2, Cmd.batch [ cmd, cmd2 ] )


eventTypeToTimelineType : EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> CurrentTimeline
eventTypeToTimelineType eventType =
    case eventType of
        UpdateFromFrontendEvent _ ->
            BackendTimeline

        UpdateFromBackendEvent { clientId } ->
            FrontendTimeline clientId

        BackendUpdateEvent _ _ ->
            BackendTimeline

        FrontendUpdateEvent clientId _ _ ->
            FrontendTimeline clientId

        TestEvent maybeClientId _ ->
            case maybeClientId of
                Just clientId ->
                    FrontendTimeline clientId

                Nothing ->
                    BackendTimeline

        BackendInitEvent _ ->
            BackendTimeline

        FrontendInitEvent clientId _ ->
            FrontendTimeline clientId

        CheckStateEvent { checkType } ->
            case checkType of
                CheckFrontendView clientId ->
                    FrontendTimeline clientId

                CheckFrontendState clientId ->
                    FrontendTimeline clientId

                CheckState ->
                    BackendTimeline

                CheckBackend ->
                    BackendTimeline

        UserInputEvent data ->
            FrontendTimeline data.clientId

        SnapshotEvent data ->
            FrontendTimeline data.clientId


isSkippable : EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> Bool
isSkippable eventType =
    case eventType of
        TestEvent _ _ ->
            True

        UserInputEvent _ ->
            True

        CheckStateEvent _ ->
            True

        UpdateFromFrontendEvent _ ->
            False

        UpdateFromBackendEvent _ ->
            False

        BackendUpdateEvent _ _ ->
            False

        FrontendUpdateEvent _ _ _ ->
            False

        BackendInitEvent _ ->
            False

        FrontendInitEvent _ _ ->
            False

        SnapshotEvent _ ->
            True


nextTimelineStep :
    Bool
    -> Int
    -> CurrentTimeline
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Maybe ( Int, Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel )
nextTimelineStep skipTestEvents stepIndex timeline test =
    if stepIndex >= Array.length test.steps then
        Nothing

    else
        Array.slice (stepIndex + 1) (Array.length test.steps) test.steps
            |> Array.foldl
                (\step state ->
                    case state of
                        Done _ ->
                            state

                        Continue index ->
                            if skipTestEvents && isSkippable step.eventType then
                                Continue (index + 1)

                            else if eventTypeToTimelineType step.eventType == timeline then
                                Done ( index, step )

                            else
                                Continue (index + 1)
                )
                (Continue (stepIndex + 1))
            |> (\a ->
                    case a of
                        Continue _ ->
                            Nothing

                        Done b ->
                            Just b
               )


previousTimelineStep :
    Bool
    -> Int
    -> CurrentTimeline
    ->
        { a
            | stepIndex : Int
            , steps : Array (Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        }
    -> Maybe ( Int, Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel )
previousTimelineStep skipTestEvents stepIndex timeline test =
    if stepIndex <= 0 then
        Nothing

    else
        Array.slice 0 stepIndex test.steps
            |> Array.foldr
                (\step state ->
                    case state of
                        Done _ ->
                            state

                        Continue index ->
                            if skipTestEvents && isSkippable step.eventType then
                                Continue (index - 1)

                            else if eventTypeToTimelineType step.eventType == timeline then
                                Done ( index, step )

                            else
                                Continue (index - 1)
                )
                (Continue (stepIndex - 1))
            |> (\a ->
                    case a of
                        Continue _ ->
                            Nothing

                        Done b ->
                            Just b
               )


type Fold c d
    = Continue c
    | Done d


currentTimeline : TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> CurrentTimeline
currentTimeline currentTest =
    Array.get currentTest.timelineIndex currentTest.timelines |> Maybe.withDefault BackendTimeline


checkCachedElmValueHelper :
    Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
checkCachedElmValueHelper event state =
    { event
        | cachedElmValue =
            case event.eventType of
                BackendUpdateEvent msg cmd3 ->
                    { diff =
                        DebugParser.valueToElmValue
                            { newSubscriptions = state.backendApp.subscriptions event.backend
                            , newModel = event.backend
                            }
                    , noDiff =
                        DebugParser.valueToElmValue
                            { backendMsg = msg
                            , cmds = cmd3
                            }
                    }
                        |> Just

                UpdateFromFrontendEvent { toBackend, cmds } ->
                    { diff =
                        DebugParser.valueToElmValue
                            { newSubscriptions = state.backendApp.subscriptions event.backend
                            , newModel = event.backend
                            }
                    , noDiff =
                        DebugParser.valueToElmValue
                            { toBackend = toBackend
                            , cmds = cmds
                            }
                    }
                        |> Just

                UpdateFromBackendEvent { clientId, toFrontend, cmds } ->
                    case Dict.get clientId event.frontends of
                        Just frontend ->
                            { diff =
                                DebugParser.valueToElmValue
                                    { newSubscriptions = state.frontendApp.subscriptions frontend.model
                                    , newModel = frontend.model
                                    }
                            , noDiff =
                                DebugParser.valueToElmValue
                                    { toFrontend = toFrontend
                                    , cmds = cmds
                                    }
                            }
                                |> Just

                        Nothing ->
                            Nothing

                FrontendUpdateEvent clientId frontendMsg cmd3 ->
                    case Dict.get clientId event.frontends of
                        Just frontend ->
                            { diff =
                                DebugParser.valueToElmValue
                                    { newSubscriptions = state.frontendApp.subscriptions frontend.model
                                    , newModel = frontend.model
                                    }
                            , noDiff =
                                DebugParser.valueToElmValue
                                    { frontendMsg = frontendMsg
                                    , cmds = cmd3
                                    }
                            }
                                |> Just

                        Nothing ->
                            Nothing

                BackendInitEvent cmd3 ->
                    { diff =
                        DebugParser.valueToElmValue
                            { newSubscriptions = state.backendApp.subscriptions event.backend
                            , newModel = event.backend
                            }
                    , noDiff = DebugParser.valueToElmValue { cmds = cmd3 }
                    }
                        |> Just

                FrontendInitEvent clientId cmd3 ->
                    case Dict.get clientId event.frontends of
                        Just frontend ->
                            { diff =
                                DebugParser.valueToElmValue
                                    { newSubscriptions = state.frontendApp.subscriptions frontend.model
                                    , newModel = frontend.model
                                    }
                            , noDiff = DebugParser.valueToElmValue { cmds = cmd3 }
                            }
                                |> Just

                        Nothing ->
                            Nothing

                TestEvent _ _ ->
                    Nothing

                CheckStateEvent _ ->
                    Nothing

                UserInputEvent _ ->
                    Nothing

                SnapshotEvent _ ->
                    Nothing
    }


updateAt : Int -> (b -> b) -> Array b -> Array b
updateAt index mapFunc array =
    case Array.get index array of
        Just item ->
            Array.set index (mapFunc item) array

        Nothing ->
            array


updateCurrentTest :
    (TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
     ->
        ( TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        , Cmd (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        )
    )
    -> Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    ->
        ( Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
        , Cmd (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        )
updateCurrentTest func model =
    case model.currentTest of
        Just currentTest ->
            let
                ( currentTest2, cmd ) =
                    func currentTest
            in
            ( { model | currentTest = Just currentTest2 }, cmd )

        Nothing ->
            ( model, Cmd.none )


view :
    Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Browser.Document (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
view model =
    { title = "Test viewer"
    , body =
        case model.tests of
            Just (Ok tests) ->
                case model.currentTest of
                    Just testView_ ->
                        case getAt testView_.index tests of
                            Just instructions ->
                                testView (Tuple.first model.windowSize) instructions testView_

                            Nothing ->
                                [ text "Invalid index for tests" ]

                    Nothing ->
                        [ overview tests model.testResults ]

            Just (Err error) ->
                [ fileLoadErrorToString error |> text ]

            Nothing ->
                [ text "Loading files for tests..." ]
    }


fileLoadErrorToString : FileLoadError -> String
fileLoadErrorToString error =
    "Failed to load \""
        ++ error.name
        ++ "\" "
        ++ (case error.error of
                HttpError Http.NetworkError ->
                    "due to a network error"

                HttpError (Http.BadUrl _) ->
                    "because the path is invalid"

                HttpError Http.Timeout ->
                    "due to a network timeout"

                HttpError (Http.BadStatus code) ->
                    "and instead got a " ++ String.fromInt code ++ " error"

                HttpError (Http.BadBody _) ->
                    "due to a bad response body"

                TextureError WebGLFix.Texture.LoadError ->
                    "due to the file not being found or a network error"

                TextureError (WebGLFix.Texture.SizeError w h) ->
                    "due to the texture being an invalid size (width: " ++ String.fromInt w ++ ", height: " ++ String.fromInt h ++ ")"
           )


getAt : Int -> List a -> Maybe a
getAt index list =
    List.drop index list |> List.head


overview :
    List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    -> List (Result TestError ())
    -> Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
overview tests testResults_ =
    List.foldl
        (\test { index, testResults, elements } ->
            { index = index + 1
            , testResults = List.drop 1 testResults
            , elements =
                Html.div
                    [ Html.Attributes.style "padding-bottom" "4px" ]
                    [ button (PressedViewTest index) (getTestName test)
                    , case testResults of
                        (Ok ()) :: _ ->
                            Html.span
                                [ Html.Attributes.style "color" "rgb(0, 200, 0)"
                                , Html.Attributes.style "padding" "4px"
                                ]
                                [ Html.text "Passed" ]

                        (Err head) :: _ ->
                            Html.span
                                [ Html.Attributes.style "color" "rgb(200, 10, 10)"
                                , Html.Attributes.style "padding" "4px"
                                ]
                                [ Html.text (testErrorToString head) ]

                        [] ->
                            Html.text ""
                    ]
                    :: elements
            }
        )
        { index = 0, testResults = testResults_, elements = [] }
        tests
        |> .elements
        |> List.reverse
        |> (::) (titleText "End to end test viewer")
        |> Html.div
            [ Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "font-family" "arial"
            , Html.Attributes.style "font-size" "16px"
            , darkBackground
            , Html.Attributes.style "height" "100vh"
            ]


darkBackground : Html.Attribute msg
darkBackground =
    Html.Attributes.style "background-color" "rgba(0,0,0,0.9)"


button : msg -> String -> Html msg
button onPress text_ =
    Html.button
        [ Html.Events.onClick onPress
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "color" "rgb(10,10,10)"
        , Html.Attributes.style "background-color" "rgb(240,240,240)"
        , Html.Attributes.style "border-width" "0px"
        , Html.Attributes.style "border-radius" "4px"
        ]
        [ Html.text text_ ]


overlayButton : msg -> String -> Html msg
overlayButton onPress text_ =
    Html.button
        [ Html.Events.onClick onPress
        , Html.Attributes.style "padding" "2px"
        , Html.Attributes.style "margin" "0px"
        , Html.Attributes.style "color" "rgb(10,10,10)"
        , Html.Attributes.style "background-color" "rgb(240,240,240)"
        , Html.Attributes.style "border-color" "rgb(250,250,250)"
        , Html.Attributes.style "border-width" "1px"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "border-style" "solid"
        , Html.Attributes.style "font-family" "arial"
        , Html.Attributes.style "font-size" "14px"
        , Html.Attributes.style "font-weight" "regular"
        , Html.Attributes.style "line-height" "1"
        ]
        [ Html.text text_ ]


overlaySelectButton : Bool -> msg -> String -> Html msg
overlaySelectButton isSelected onPress text_ =
    Html.button
        [ Html.Events.onClick onPress
        , Html.Attributes.style "padding" "2px"
        , Html.Attributes.style "color" "rgb(10,10,10)"
        , Html.Attributes.style
            "background-color"
            (if isSelected then
                "rgb(180,200,255)"

             else
                "rgb(240,240,240)"
            )
        , Html.Attributes.style "border-color" "rgb(250,250,250)"
        , Html.Attributes.style "border-width" "1px"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "border-style" "solid"
        , Html.Attributes.style "font-family" "arial"
        , Html.Attributes.style "font-size" "14px"
        , Html.Attributes.style "font-weight" "regular"
        , Html.Attributes.style "line-height" "1"
        ]
        [ Html.text text_ ]


text : String -> Html msg
text text_ =
    Html.div
        [ Html.Attributes.style "padding" "4px"
        ]
        [ Html.text text_ ]


titleText : String -> Html msg
titleText text_ =
    Html.h1
        [ Html.Attributes.style "font-size" "20px"
        , defaultFontColor
        ]
        [ Html.text text_ ]


defaultFontColor : Html.Attribute msg
defaultFontColor =
    Html.Attributes.style "color" "rgb(240,240,240)"


getState :
    Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
getState instructions =
    case instructions of
        NextStep _ instructions_ ->
            getState instructions_

        AndThen _ instructions_ ->
            getState instructions_

        Start state ->
            state


getTestName : Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel -> String
getTestName instructions =
    case instructions of
        NextStep _ instructions_ ->
            getTestName instructions_

        AndThen _ instructions_ ->
            getTestName instructions_

        Start state ->
            state.testName


modelView :
    RegularDict.Dict (List String) CollapsedField
    -> Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
modelView collapsedFields event =
    case event.cachedElmValue of
        Just elmValue ->
            Html.div
                []
                [ Effect.TreeView.treeView treeViewConfig 0 [] collapsedFields elmValue.diff
                , Effect.TreeView.treeView treeViewConfig 0 [] collapsedFields elmValue.noDiff
                ]

        Nothing ->
            Html.text "Failed to show model"


treeViewConfig : Effect.TreeView.MsgConfig (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
treeViewConfig =
    { pressedExpandField = PressedExpandField
    , pressedCollapseField = PressedCollapseField
    }


refineElmValue : ElmValue -> ElmValue
refineElmValue value =
    case value of
        Plain _ ->
            value

        Expandable expandableValue ->
            (case expandableValue of
                ElmSequence sequenceType elmValues ->
                    List.map refineElmValue elmValues |> ElmSequence sequenceType

                ElmType variant elmValues ->
                    case ( variant, elmValues ) of
                        ( "D", [ Expandable (ElmSequence SeqList list) ] ) ->
                            List.filterMap
                                (\a ->
                                    case a of
                                        Expandable (ElmSequence SeqTuple [ key, value2 ]) ->
                                            Just ( refineElmValue key, refineElmValue value2 )

                                        _ ->
                                            Nothing
                                )
                                list
                                |> ElmDict

                        _ ->
                            ElmType variant (List.map refineElmValue elmValues)

                ElmRecord fields ->
                    List.map (\( field, value2 ) -> ( field, refineElmValue value2 )) fields |> ElmRecord

                ElmDict list ->
                    List.map (\( key, value2 ) -> ( refineElmValue key, refineElmValue value2 )) list
                        |> ElmDict
            )
                |> Expandable


blockArrowKeys : List (Html.Attribute (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
blockArrowKeys =
    [ Html.Events.preventDefaultOn "keydown" (decodeArrows |> Json.Decode.map (\_ -> ( NoOp, True )))
    , Html.Attributes.tabindex -1
    ]


modelDiffView :
    RegularDict.Dict (List String) CollapsedField
    -> Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
modelDiffView collapsedFields step previousStep =
    case ( step.cachedElmValue, previousStep.cachedElmValue ) of
        ( Just ok, Just previous ) ->
            Html.div
                []
                [ Effect.TreeView.treeViewDiff treeViewConfig 0 [] collapsedFields previous.diff ok.diff
                , Effect.TreeView.treeView treeViewConfig 0 [] collapsedFields ok.noDiff
                ]

        _ ->
            Html.text "Failed to show frontend model"


currentStepText :
    Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
currentStepText currentStep testView_ =
    let
        fullMsg : String
        fullMsg =
            case currentStep.eventType of
                TestEvent _ name ->
                    name

                UpdateFromFrontendEvent { toBackend } ->
                    "UpdateFromFrontend: " ++ Debug.toString toBackend

                UpdateFromBackendEvent { toFrontend } ->
                    "UpdateFromBackend: " ++ Debug.toString toFrontend

                BackendUpdateEvent backendMsg _ ->
                    "BackendUpdate: " ++ Debug.toString backendMsg

                FrontendUpdateEvent _ frontendMsg _ ->
                    "FrontendUpdate: " ++ Debug.toString frontendMsg

                BackendInitEvent _ ->
                    "BackendInit"

                FrontendInitEvent clientId _ ->
                    "FrontendInitEvent: " ++ Effect.Lamdera.clientIdToString clientId

                CheckStateEvent { checkType } ->
                    case checkType of
                        CheckFrontendView _ ->
                            "Check frontend view"

                        CheckFrontendState _ ->
                            "Check frontend state"

                        CheckState ->
                            "Check global state"

                        CheckBackend ->
                            "Check backend"

                UserInputEvent data ->
                    case data.inputType of
                        UserClicksButton htmlId ->
                            "Click \"" ++ Effect.Browser.Dom.idToString htmlId ++ "\" button"

                        UserInputsText htmlId text2 ->
                            "Type \"" ++ text2 ++ "\" into \"" ++ Effect.Browser.Dom.idToString htmlId ++ "\" input"

                        UserPressesKey htmlId { keyCode } ->
                            "Press key with key code " ++ String.fromInt keyCode ++ " into \"" ++ Effect.Browser.Dom.idToString htmlId ++ "\" input"

                        UserClicksLink { href } ->
                            "Press link leading to " ++ href

                SnapshotEvent data ->
                    "Snapshot view with name " ++ data.name
    in
    Html.div
        [ Html.Attributes.style "padding" "4px", Html.Attributes.title fullMsg ]
        [ " "
            ++ String.fromInt (testView_.stepIndex + 1)
            ++ "/"
            ++ String.fromInt (Array.length testView_.steps)
            ++ (" " ++ ellipsis2 100 fullMsg)
            |> Html.text
        ]


ellipsis2 : Int -> String -> String
ellipsis2 maxChars text2 =
    if String.length text2 > maxChars then
        String.left (maxChars - 3) text2 ++ "..."

    else
        text2


type alias TimelineViewData toBackend frontendMsg frontendModel toFrontend backendMsg backendModel =
    { events : List (Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
    , columnStart : Int
    , columnEnd : Int
    , rowIndex : Int
    }


unselectedTimelineColor =
    "#626262"


addTimelineEvent :
    Int
    -> { previousStep : Maybe Int, currentStep : Maybe Int }
    -> Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    ->
        { columnIndex : Int
        , dict : Dict CurrentTimeline (TimelineViewData toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        }
    ->
        { columnIndex : Int
        , dict : Dict CurrentTimeline (TimelineViewData toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
        }
addTimelineEvent currentTimelineIndex { previousStep, currentStep } event state =
    let
        arrowHelper : Int -> Int -> Int -> List (Html msg)
        arrowHelper rowIndexStart rowIndexEnd stepIndex =
            let
                x0 =
                    stepIndex * timelineColumnWidth + timelineColumnWidth // 2 |> toFloat

                y0 =
                    rowIndexStart * timelineRowHeight + timelineRowHeight // 4 |> toFloat

                x1 =
                    state.columnIndex * timelineColumnWidth + timelineColumnWidth // 2 |> toFloat

                y1 =
                    rowIndexEnd * timelineRowHeight + timelineRowHeight // 4 |> toFloat

                length =
                    (x1 - x0) ^ 2 + (y1 - y0) ^ 2 |> sqrt

                length2 =
                    (length - 4) / length

                color2 : String
                color2 =
                    if currentTimelineIndex == rowIndexStart || currentTimelineIndex == rowIndexEnd then
                        "white"

                    else
                        unselectedTimelineColor
            in
            [ arrowSvg color2 x0 y0 (length2 * (x1 - x0) + x0) (length2 * (y1 - y0) + y0) ]

        arrows : Int -> List (Html msg)
        arrows rowIndex =
            case event.eventType of
                FrontendUpdateEvent _ _ _ ->
                    []

                UpdateFromBackendEvent data ->
                    arrowHelper 0 rowIndex data.stepIndex

                UpdateFromFrontendEvent data ->
                    case data.stepIndex of
                        Just stepIndex ->
                            case Dict.get (FrontendTimeline data.clientId) state.dict of
                                Just timeline ->
                                    arrowHelper timeline.rowIndex rowIndex stepIndex

                                Nothing ->
                                    []

                        Nothing ->
                            []

                BackendUpdateEvent _ _ ->
                    []

                TestEvent _ _ ->
                    []

                BackendInitEvent _ ->
                    []

                FrontendInitEvent _ _ ->
                    []

                CheckStateEvent _ ->
                    []

                UserInputEvent _ ->
                    []

                SnapshotEvent _ ->
                    []
    in
    { columnIndex = state.columnIndex + 1
    , dict =
        Dict.update
            (eventTypeToTimelineType event.eventType)
            (\maybeTimeline ->
                let
                    color : Int -> String
                    color rowIndex =
                        if currentTimelineIndex /= rowIndex then
                            unselectedTimelineColor

                        else if previousStep == Just state.columnIndex then
                            "red"

                        else if currentStep == Just state.columnIndex then
                            "green"

                        else
                            "white"
                in
                (case maybeTimeline of
                    Just timeline ->
                        { events =
                            arrows timeline.rowIndex
                                ++ eventIcon (color timeline.rowIndex) event.eventType state.columnIndex timeline.rowIndex
                                ++ timeline.events
                        , columnStart = timeline.columnStart
                        , columnEnd = state.columnIndex
                        , rowIndex = timeline.rowIndex
                        }

                    Nothing ->
                        let
                            rowIndex : Int
                            rowIndex =
                                Dict.size state.dict
                        in
                        { events =
                            arrows rowIndex ++ eventIcon (color rowIndex) event.eventType state.columnIndex rowIndex
                        , columnStart = state.columnIndex
                        , columnEnd = state.columnIndex
                        , rowIndex = rowIndex
                        }
                )
                    |> Just
            )
            state.dict
    }


arrowSvg : String -> Float -> Float -> Float -> Float -> Html msg
arrowSvg color x0 y0 x1 y1 =
    let
        length =
            (x1 - x0) ^ 2 + (y1 - y0) ^ 2 |> sqrt

        offset =
            (length - 6) / length

        x2 =
            offset * (x1 - x0) + x0

        y2 =
            offset * (y1 - y0) + y0

        arrowWidthScalar =
            0.666

        x3 =
            -(y1 - y2) * arrowWidthScalar + x2

        y3 =
            (x1 - x2) * arrowWidthScalar + y2

        x4 =
            (y1 - y2) * arrowWidthScalar + x2

        y4 =
            -(x1 - x2) * arrowWidthScalar + y2

        maxX =
            max x0 x1 + 10

        maxY =
            max y0 y1 + 10
    in
    Svg.svg
        [ Svg.Attributes.width (String.fromFloat maxX)
        , Svg.Attributes.height (String.fromFloat maxY)
        , "0 0 " ++ String.fromFloat maxX ++ " " ++ String.fromFloat maxY |> Svg.Attributes.viewBox
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "pointer-events" "none"
        ]
        [ Svg.line
            [ Svg.Attributes.x1 (String.fromFloat x0)
            , Svg.Attributes.y1 (String.fromFloat y0)
            , Svg.Attributes.x2 (String.fromFloat x2)
            , Svg.Attributes.y2 (String.fromFloat y2)
            , Svg.Attributes.width "20"
            , Html.Attributes.style "stroke" color
            , Html.Attributes.style "stroke-width" "2"
            ]
            []
        , Svg.polygon
            [ Html.Attributes.style "fill" color
            , (String.fromFloat x3 ++ "," ++ String.fromFloat y3)
                ++ " "
                ++ (String.fromFloat x1 ++ "," ++ String.fromFloat y1)
                ++ " "
                ++ (String.fromFloat x4 ++ "," ++ String.fromFloat y4)
                |> Svg.Attributes.points
            ]
            []
        ]


timelineRowHeight : number
timelineRowHeight =
    32


timelineView :
    Int
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
timelineView windowWidth testView_ =
    let
        sideBarWidth =
            64

        leftPadding =
            4

        currentTimeline_ : CurrentTimeline
        currentTimeline_ =
            currentTimeline testView_

        timelines : List ( CurrentTimeline, TimelineViewData toBackend frontendMsg frontendModel toFrontend backendMsg backendModel )
        timelines =
            getTimelines testView_.timelineIndex testView_
    in
    Html.div
        []
        [ Html.div
            [ Html.Attributes.style "display" "inline-block"
            , Html.Attributes.style "position" "relative"
            , Html.Attributes.style "width" (px (sideBarWidth - leftPadding))
            , Html.Attributes.style "height" (px (List.length timelines * timelineRowHeight))
            , Html.Attributes.style "padding-left" (px leftPadding)
            , Html.Attributes.style "font-size" "14px"
            , Html.Attributes.style "box-sizing" "unset"
            ]
            (List.map
                (\( timelineType, timeline ) ->
                    Html.button
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "top" (px (timeline.rowIndex * timelineRowHeight - 4))
                        , Html.Attributes.style
                            "color"
                            (if currentTimeline_ == timelineType then
                                "white"

                             else
                                unselectedTimelineColor
                            )
                        , darkBackground
                        , Html.Attributes.style "border-width" "0"
                        , Html.Attributes.style "margin" "0"
                        , Html.Attributes.style "padding" "4px 0 4px 0"
                        , Html.Events.onClick (PressedTimeline timelineType)
                        ]
                        [ Html.text
                            (case timelineType of
                                BackendTimeline ->
                                    "Backend"

                                FrontendTimeline clientId ->
                                    Effect.Lamdera.clientIdToString clientId
                            )
                        ]
                )
                timelines
            )
        , timelineViewHelper
            (windowWidth - sideBarWidth - 1 {- The extra minus 1 is to account for rounding errors -})
            testView_
            timelines
        ]


timelineViewHelper :
    Int
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> List ( CurrentTimeline, TimelineViewData toBackend frontendMsg frontendModel toFrontend backendMsg backendModel )
    -> Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
timelineViewHelper width testView_ timelines =
    let
        maxColumnEnd : Int
        maxColumnEnd =
            List.map (\( _, timeline ) -> timeline.columnEnd) timelines |> List.maximum |> Maybe.withDefault 0
    in
    List.concatMap
        (\( timelineType, timeline ) ->
            Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "left" (px (timeline.columnStart * timelineColumnWidth + timelineColumnWidth // 2))
                , Html.Attributes.style "top" (px (timeline.rowIndex * timelineRowHeight + 7))
                , Html.Attributes.style "height" "2px"
                , Html.Attributes.style "pointer-events" "none"
                , Html.Attributes.style
                    "width"
                    (case timelineType of
                        FrontendTimeline _ ->
                            px ((timeline.columnEnd - timeline.columnStart) * timelineColumnWidth + timelineColumnWidth // 4)

                        BackendTimeline ->
                            px ((maxColumnEnd - timeline.columnStart) * timelineColumnWidth + timelineColumnWidth // 4)
                    )
                , Html.Attributes.style
                    "background-color"
                    (if testView_.timelineIndex == timeline.rowIndex then
                        "white"

                     else
                        unselectedTimelineColor
                    )
                ]
                []
                :: timeline.events
        )
        timelines
        |> (\a ->
                timelineCss
                    :: List.map
                        (\index ->
                            Html.div
                                ([ Html.Attributes.style "left" (px (index * timelineColumnWidth))
                                 , Html.Attributes.style "width" (px timelineColumnWidth)
                                 , Html.Attributes.style "height" (px (List.length timelines * timelineRowHeight))
                                 , Html.Attributes.style "position" "absolute"
                                 , Html.Events.onClick (PressedTimelineEvent index)
                                 ]
                                    ++ (if index == testView_.stepIndex then
                                            [ Html.Attributes.id timelineEventId
                                            , Html.Attributes.style "background-color" "rgba(255,255,255,0.4)"
                                            ]

                                        else
                                            []
                                       )
                                )
                                []
                        )
                        (List.range 0 (Array.length testView_.steps - 1))
                    ++ a
           )
        |> Html.div
            [ Html.Attributes.style "width" (px width)
            , Html.Attributes.style "height" (px (List.length timelines * timelineRowHeight))
            , Html.Attributes.style "position" "relative"
            , Html.Attributes.style "overflow-x" "auto"
            , Html.Attributes.style "overflow-y" "clip"
            , Html.Events.preventDefaultOn "keydown" (decodeArrows |> Json.Decode.map (\_ -> ( NoOp, True )))
            , Html.Attributes.tabindex -1
            , Html.Attributes.style "display" "inline-block"
            , Html.Attributes.id timelineContainerId
            ]


timelineContainerId : String
timelineContainerId =
    "timelineContainer123"


hasToBackendCmds : Command FrontendOnly toBackend frontendMsg -> Bool
hasToBackendCmds cmd =
    case cmd of
        Batch commands ->
            List.any hasToBackendCmds commands

        SendToBackend _ ->
            True

        _ ->
            False


hasToFrontendCmds :
    Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Command BackendOnly toFrontend backendMsg
    -> Set String
hasToFrontendCmds event cmd =
    case cmd of
        Batch commands ->
            List.foldl (\cmd2 set -> Set.union set (hasToFrontendCmds event cmd2)) Set.empty commands

        SendToFrontend (Effect.Internal.ClientId clientId) _ ->
            if Dict.member (Effect.Lamdera.clientIdFromString clientId) event.frontends then
                Set.singleton clientId

            else
                Set.empty

        SendToFrontends (Effect.Internal.SessionId sessionId) _ ->
            Dict.toList event.frontends
                |> List.filterMap
                    (\( clientId, frontend ) ->
                        if Effect.Lamdera.sessionIdToString frontend.sessionId == sessionId then
                            Just (Effect.Lamdera.clientIdToString clientId)

                        else
                            Nothing
                    )
                |> Set.fromList

        Broadcast _ ->
            Dict.keys event.frontends |> List.map Effect.Lamdera.clientIdToString |> Set.fromList

        _ ->
            Set.empty


timelineEventId : String
timelineEventId =
    "currentEvent123"


px value =
    String.fromInt value ++ "px"


timelineCss =
    Html.node "style"
        []
        [ Html.text
            """
.circle {
    width: 8px;
    height: 8px;
    margin: 3px;
    border-radius: 8px;
    pointer-events: none;
    position: absolute;
}
.big-circle {
    width: 12px;
    height: 12px;
    margin: 1px;
    border-radius: 8px;
    pointer-events: none;
    position: absolute;
}
    """
        ]


timelineColumnWidth =
    14


eventIcon :
    String
    -> EventType toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Int
    -> Int
    -> List (Html msg)
eventIcon color eventType columnIndex rowIndex =
    let
        circleHelper : String -> Html msg
        circleHelper class =
            Html.div
                [ Html.Attributes.style "background-color" color
                , Html.Attributes.style "left" (px (columnIndex * timelineColumnWidth))
                , Html.Attributes.style "top" (px (rowIndex * timelineRowHeight + 1))
                , Html.Attributes.class class
                ]
                []
    in
    case eventType of
        FrontendUpdateEvent _ _ _ ->
            [ circleHelper "circle" ]

        UpdateFromFrontendEvent _ ->
            [ circleHelper "circle" ]

        UpdateFromBackendEvent _ ->
            [ circleHelper "circle" ]

        BackendUpdateEvent _ _ ->
            [ circleHelper "circle" ]

        TestEvent _ _ ->
            [ circleHelper "big-circle" ]

        BackendInitEvent _ ->
            [ circleHelper "circle" ]

        FrontendInitEvent _ _ ->
            [ circleHelper "circle" ]

        CheckStateEvent { isSuccessful } ->
            [ magnifyingGlassSvg color (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight)
            ]
                ++ (if isSuccessful then
                        []

                    else
                        [ xSvg "red" (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight) ]
                   )

        UserInputEvent data ->
            (case data.inputType of
                UserClicksButton _ ->
                    [ cursorSvg color (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight) ]

                UserInputsText _ _ ->
                    [ cursorTextSvg color (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight) ]

                UserPressesKey _ { keyCode } ->
                    [ circleHelper "big-circle" ]

                UserClicksLink _ ->
                    [ simpleLinkSvg color (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight) ]
            )
                ++ (if data.isSuccessful then
                        []

                    else
                        [ xSvg "red" (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight) ]
                   )

        SnapshotEvent data ->
            [ cameraSvg color (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight) ]
                ++ (if data.isSuccessful then
                        []

                    else
                        [ xSvg "red" (columnIndex * timelineColumnWidth) (rowIndex * timelineRowHeight) ]
                   )


cameraSvg : String -> Int -> Int -> Html msg
cameraSvg color left top =
    Svg.svg
        [ Svg.Attributes.width (String.fromInt timelineColumnWidth)
        , Html.Attributes.style "left" (px left)
        , Html.Attributes.style "top" (px top)
        , Html.Attributes.style "position" "absolute"
        , Svg.Attributes.viewBox "0 -10 250 400"
        , Html.Attributes.style "pointer-events" "none"
        ]
        [ Svg.path
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "60"
            , Svg.Attributes.d "M208,52H182.42L170,33.34A12,12,0,0,0,160,28H96a12,12,0,0,0-10,5.34L73.57,52H48A28,28,0,0,0,20,80V192a28,28,0,0,0,28,28H208a28,28,0,0,0,28-28V80A28,28,0,0,0,208,52Zm4,140a4,4,0,0,1-4,4H48a4,4,0,0,1-4-4V80a4,4,0,0,1,4-4H80a12,12,0,0,0,10-5.34L102.42,52h51.15L166,70.66A12,12,0,0,0,176,76h32a4,4,0,0,1,4,4ZM128,84a48,48,0,1,0,48,48A48.05,48.05,0,0,0,128,84Zm0,72a24,24,0,1,1,24-24A24,24,0,0,1,128,156Z"
            ]
            []
        , Svg.path
            [ Svg.Attributes.fill color
            , Svg.Attributes.d "M208,52H182.42L170,33.34A12,12,0,0,0,160,28H96a12,12,0,0,0-10,5.34L73.57,52H48A28,28,0,0,0,20,80V192a28,28,0,0,0,28,28H208a28,28,0,0,0,28-28V80A28,28,0,0,0,208,52Zm4,140a4,4,0,0,1-4,4H48a4,4,0,0,1-4-4V80a4,4,0,0,1,4-4H80a12,12,0,0,0,10-5.34L102.42,52h51.15L166,70.66A12,12,0,0,0,176,76h32a4,4,0,0,1,4,4ZM128,84a48,48,0,1,0,48,48A48.05,48.05,0,0,0,128,84Zm0,72a24,24,0,1,1,24-24A24,24,0,0,1,128,156Z"
            ]
            []
        ]


simpleLinkSvg : String -> Int -> Int -> Svg msg
simpleLinkSvg color left top =
    Svg.svg
        [ Svg.Attributes.width (String.fromInt timelineColumnWidth)
        , Html.Attributes.style "left" (px left)
        , Html.Attributes.style "top" (px top)
        , Html.Attributes.style "position" "absolute"
        , Svg.Attributes.viewBox "10 0 240 400"
        , Html.Attributes.style "pointer-events" "none"
        , Html.Attributes.style "transform" "scale(-1, 1)"
        ]
        [ Svg.path
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "60"
            , Svg.Attributes.d "M87.5,151.52l64-64a12,12,0,0,1,17,17l-64,64a12,12,0,0,1-17-17Zm131-114a60.08,60.08,0,0,0-84.87,0L103.51,67.61a12,12,0,0,0,17,17l30.07-30.06a36,36,0,0,1,50.93,50.92L171.4,135.52a12,12,0,1,0,17,17l30.08-30.06A60.09,60.09,0,0,0,218.45,37.55ZM135.52,171.4l-30.07,30.08a36,36,0,0,1-50.92-50.93l30.06-30.07a12,12,0,0,0-17-17L37.55,133.58a60,60,0,0,0,84.88,84.87l30.06-30.07a12,12,0,0,0-17-17Z"
            ]
            []
        , Svg.path
            [ Svg.Attributes.fill color
            , Svg.Attributes.d "M87.5,151.52l64-64a12,12,0,0,1,17,17l-64,64a12,12,0,0,1-17-17Zm131-114a60.08,60.08,0,0,0-84.87,0L103.51,67.61a12,12,0,0,0,17,17l30.07-30.06a36,36,0,0,1,50.93,50.92L171.4,135.52a12,12,0,1,0,17,17l30.08-30.06A60.09,60.09,0,0,0,218.45,37.55ZM135.52,171.4l-30.07,30.08a36,36,0,0,1-50.92-50.93l30.06-30.07a12,12,0,0,0-17-17L37.55,133.58a60,60,0,0,0,84.88,84.87l30.06-30.07a12,12,0,0,0-17-17Z"
            ]
            []
        ]


cursorTextSvg : String -> Int -> Int -> Svg msg
cursorTextSvg color left top =
    Svg.svg
        [ Svg.Attributes.width (String.fromInt timelineColumnWidth)
        , Html.Attributes.style "left" (px left)
        , Html.Attributes.style "top" (px top)
        , Html.Attributes.style "position" "absolute"
        , Svg.Attributes.viewBox "10 -10 240 400"
        , Html.Attributes.style "pointer-events" "none"
        ]
        [ Svg.path
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "60"
            , Svg.Attributes.d "M188,208a12,12,0,0,1-12,12H160a43.86,43.86,0,0,1-32-13.85A43.86,43.86,0,0,1,96,220H80a12,12,0,0,1,0-24H96a20,20,0,0,0,20-20V140H104a12,12,0,0,1,0-24h12V80A20,20,0,0,0,96,60H80a12,12,0,0,1,0-24H96a43.86,43.86,0,0,1,32,13.85A43.86,43.86,0,0,1,160,36h16a12,12,0,0,1,0,24H160a20,20,0,0,0-20,20v36h12a12,12,0,0,1,0,24H140v36a20,20,0,0,0,20,20h16A12,12,0,0,1,188,208Z"
            ]
            []
        , Svg.path
            [ Svg.Attributes.fill color
            , Svg.Attributes.d "M188,208a12,12,0,0,1-12,12H160a43.86,43.86,0,0,1-32-13.85A43.86,43.86,0,0,1,96,220H80a12,12,0,0,1,0-24H96a20,20,0,0,0,20-20V140H104a12,12,0,0,1,0-24h12V80A20,20,0,0,0,96,60H80a12,12,0,0,1,0-24H96a43.86,43.86,0,0,1,32,13.85A43.86,43.86,0,0,1,160,36h16a12,12,0,0,1,0,24H160a20,20,0,0,0-20,20v36h12a12,12,0,0,1,0,24H140v36a20,20,0,0,0,20,20h16A12,12,0,0,1,188,208Z"
            ]
            []
        ]


cursorSvg : String -> Int -> Int -> Svg msg
cursorSvg color left top =
    Svg.svg
        [ Svg.Attributes.width (String.fromInt timelineColumnWidth)
        , Html.Attributes.style "left" (px left)
        , Html.Attributes.style "top" (px top)
        , Html.Attributes.style "position" "absolute"
        , Svg.Attributes.viewBox "10 0 240 400"
        , Html.Attributes.style "pointer-events" "none"
        ]
        [ Svg.path
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "60"
            , Svg.Attributes.d "M224.15,179.17l-46.83-46.82,37.93-13.51.76-.3a20,20,0,0,0-1.76-37.27L54.16,29A20,20,0,0,0,29,54.16L81.27,214.24A20,20,0,0,0,118.54,216c.11-.25.21-.5.3-.76l13.51-37.92,46.83,46.82a20,20,0,0,0,28.28,0l16.69-16.68A20,20,0,0,0,224.15,179.17Zm-30.83,25.17-48.48-48.48A20,20,0,0,0,130.7,150a20.66,20.66,0,0,0-3.74.35A20,20,0,0,0,112.35,162c-.11.25-.21.5-.3.76L100.4,195.5,54.29,54.29l141.21,46.1-32.71,11.66c-.26.09-.51.19-.76.3a20,20,0,0,0-6.17,32.48h0l48.49,48.48Z"
            ]
            []
        , Svg.path
            [ Svg.Attributes.fill color
            , Svg.Attributes.d "M224.15,179.17l-46.83-46.82,37.93-13.51.76-.3a20,20,0,0,0-1.76-37.27L54.16,29A20,20,0,0,0,29,54.16L81.27,214.24A20,20,0,0,0,118.54,216c.11-.25.21-.5.3-.76l13.51-37.92,46.83,46.82a20,20,0,0,0,28.28,0l16.69-16.68A20,20,0,0,0,224.15,179.17Zm-30.83,25.17-48.48-48.48A20,20,0,0,0,130.7,150a20.66,20.66,0,0,0-3.74.35A20,20,0,0,0,112.35,162c-.11.25-.21.5-.3.76L100.4,195.5,54.29,54.29l141.21,46.1-32.71,11.66c-.26.09-.51.19-.76.3a20,20,0,0,0-6.17,32.48h0l48.49,48.48Z"
            ]
            []
        ]


{-| Original SVG from <https://upload.wikimedia.org/wikipedia/commons/5/55/Magnifying_glass_icon.svg>
-}
magnifyingGlassSvg : String -> Int -> Int -> Svg msg
magnifyingGlassSvg color left top =
    Svg.svg
        [ Svg.Attributes.width (String.fromInt timelineColumnWidth)
        , Html.Attributes.style "left" (px left)
        , Html.Attributes.style "top" (px top)
        , Html.Attributes.style "position" "absolute"
        , Svg.Attributes.viewBox "-60 -60 600 700"
        , Html.Attributes.style "pointer-events" "none"
        ]
        [ Svg.path
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "200"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.d "m280,278a153,153 0 1,0-2,2l170,170m-91-117 110,110-26,26-110-110"
            ]
            []
        , Svg.path
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke color
            , Svg.Attributes.strokeWidth "80"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.d "m280,278a153,153 0 1,0-2,2l170,170m-91-117 110,110-26,26-110-110"
            ]
            []
        ]


xSvg : String -> Int -> Int -> Svg msg
xSvg color left top =
    Svg.svg
        [ Svg.Attributes.width (String.fromInt timelineColumnWidth)
        , Html.Attributes.style "left" (px left)
        , Html.Attributes.style "top" (px top)
        , Html.Attributes.style "position" "absolute"
        , Svg.Attributes.viewBox "-70 10 300 300"
        , Html.Attributes.style "pointer-events" "none"
        ]
        [ Svg.path
            [ Svg.Attributes.stroke color
            , Svg.Attributes.strokeWidth "30"
            , Svg.Attributes.d "M208.49,191.51a12,12,0,0,1-17,17L128,145,64.49,208.49a12,12,0,0,1-17-17L111,128,47.51,64.49a12,12,0,0,1,17-17L128,111l63.51-63.52a12,12,0,0,1,17,17L145,128Z"
            ]
            []
        ]


currentAndPreviousStepIndex :
    TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> { previousStep : Maybe Int, currentStep : Maybe Int }
currentAndPreviousStepIndex testView_ =
    case previousTimelineStep True (testView_.stepIndex + 1) (currentTimeline testView_) testView_ of
        Just ( currentIndex, _ ) ->
            { currentStep = Just currentIndex
            , previousStep =
                previousTimelineStep True currentIndex (currentTimeline testView_) testView_ |> Maybe.map Tuple.first
            }

        Nothing ->
            { previousStep = Nothing, currentStep = Nothing }


getTimelines :
    Int
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> List ( CurrentTimeline, TimelineViewData toBackend frontendMsg frontendModel toFrontend backendMsg backendModel )
getTimelines timelineIndex testView_ =
    let
        currentAndPreviousStepIndex2 : { previousStep : Maybe Int, currentStep : Maybe Int }
        currentAndPreviousStepIndex2 =
            currentAndPreviousStepIndex testView_
    in
    Array.foldl
        (addTimelineEvent
            timelineIndex
            { currentAndPreviousStepIndex2
                | previousStep =
                    if testView_.showModel then
                        currentAndPreviousStepIndex2.previousStep

                    else
                        Nothing
            }
        )
        { columnIndex = 0, dict = Dict.singleton BackendTimeline { events = [], columnStart = 0, columnEnd = 0, rowIndex = 0 } }
        testView_.steps
        |> .dict
        |> Dict.toList


getTimelines2 :
    Array (Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    -> Array CurrentTimeline
getTimelines2 steps =
    Array.foldl
        (\event dict ->
            Dict.update
                (eventTypeToTimelineType event.eventType)
                (\maybe ->
                    case maybe of
                        Just _ ->
                            maybe

                        Nothing ->
                            Dict.size dict |> Just
                )
                dict
        )
        Dict.empty
        steps
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> Array.fromList


testView :
    Int
    -> Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> List (Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
testView windowWidth instructions testView_ =
    let
        currentAndPreviousStep : { previousStep : Maybe Int, currentStep : Maybe Int }
        currentAndPreviousStep =
            currentAndPreviousStepIndex testView_
    in
    case Array.get testView_.stepIndex testView_.steps of
        Just currentStep ->
            if testView_.showModel then
                let
                    overlayHeight : Int
                    overlayHeight =
                        90 + Array.length testView_.timelines * timelineRowHeight
                in
                [ testOverlay windowWidth testView_ currentStep
                , Html.div
                    [ Html.Attributes.style "font-size" "14px"
                    , Html.Attributes.style
                        "padding"
                        (case testView_.overlayPosition of
                            Top ->
                                px overlayHeight ++ " 4px 4px 4px"

                            Bottom ->
                                "4px 4px " ++ px overlayHeight ++ " 4px"
                        )
                    , darkBackground
                    , defaultFontColor
                    , Html.Attributes.style "font-family" "arial"
                    , Html.Attributes.style "white-space" "pre"
                    , Html.Attributes.style "min-height" "100vh"
                    ]
                    [ case
                        ( Maybe.andThen (\a -> Array.get a testView_.steps) currentAndPreviousStep.currentStep
                        , Maybe.andThen (\a -> Array.get a testView_.steps) currentAndPreviousStep.previousStep
                        )
                      of
                        ( Just currentStep2, Just previousStep ) ->
                            Html.Lazy.lazy3 modelDiffView testView_.collapsedFields currentStep2 previousStep

                        ( Just currentStep2, Nothing ) ->
                            Html.Lazy.lazy2 modelView testView_.collapsedFields currentStep2

                        _ ->
                            centeredText "No model to show"
                    ]
                ]

            else
                let
                    state : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
                    state =
                        getState instructions
                in
                testOverlay windowWidth testView_ currentStep
                    :: (case currentTimeline testView_ of
                            FrontendTimeline clientId ->
                                case Maybe.andThen (\a -> Array.get a testView_.steps) currentAndPreviousStep.currentStep of
                                    Just currentStep2 ->
                                        case Dict.get clientId currentStep2.frontends of
                                            Just frontend ->
                                                state.frontendApp.view frontend.model |> .body |> List.map (Html.map (\_ -> NoOp))

                                            Nothing ->
                                                []

                                    Nothing ->
                                        [ centeredText "Frontend not connected" ]

                            BackendTimeline ->
                                [ centeredText "There is no view to display for the backend" ]
                       )

        Nothing ->
            []


centeredText text2 =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "45%"
        , Html.Attributes.style "top" "400px"
        ]
        [ Html.text text2 ]


testOverlay :
    Int
    -> TestView toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Event toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Html (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
testOverlay windowWidth testView_ currentStep =
    Html.div
        [ Html.Attributes.style "font-family" "arial"
        , Html.Attributes.style "font-size" "14px"
        , defaultFontColor
        , Html.Attributes.style "position" "fixed"
        , darkBackground
        , Html.Attributes.style "z-index" "9999"
        , Html.Attributes.style "width" "100vw"
        , case testView_.overlayPosition of
            Top ->
                Html.Attributes.style "top" "0"

            Bottom ->
                Html.Attributes.style "bottom" "0"
        ]
        [ Html.div
            [ Html.Attributes.style "padding" "4px" ]
            [ overlayButton PressedBackToOverview "Close test"
            , Html.div [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "padding" "4px" ] []
            , overlayButton PressedToggleOverlayPosition "Move"
            , Html.div [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "padding" "4px" ] []
            , if testView_.showModel then
                overlayButton PressedHideModel "Hide model"

              else
                overlayButton PressedShowModel "Show model"
            , Html.div
                [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "padding" "4px" ]
                [ Html.text testView_.testName ]
            ]
        , timelineView windowWidth testView_
        , currentStepText currentStep testView_
        , Html.div
            [ Html.Attributes.style "color" "rgb(200, 10, 10)", Html.Attributes.style "padding" "4px" ]
            (List.map (testErrorToString >> text) currentStep.testErrors)
        ]


ellipsis : Int -> String -> Html msg
ellipsis width text_ =
    Html.div
        [ Html.Attributes.style "white-space" "nowrap"
        , Html.Attributes.style "text-overflow" "ellipsis"
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "overflow-x" "hidden"
        , Html.Attributes.style "padding" "4px"
        ]
        [ Html.text text_ ]


{-| -}
type alias ViewerWith a =
    { cmds : Task.Task FileLoadError a }


{-| View your end-to-end tests in a elm reactor style app.

    import Effect.Test

    main =
        Effect.Test.viewerWith
            (\image jsonData ->
                [{- End to end tests go here -}]
            )
            |> Effect.Test.addBytesFile "/test.png"
            |> Effect.Test.addBytesFile "/data.json"
            |> Effect.Test.startViewer

-}
viewerWith : a -> ViewerWith a
viewerWith a =
    { cmds = Task.succeed a }


{-| View your end-to-end tests in a elm reactor style app.

    import Effect.Test

    main =
        Effect.Test.viewerWith
            (\image jsonData ->
                [{- End to end tests go here -}]
            )
            |> Effect.Test.addBytesFile "/test.png"
            |> Effect.Test.addBytesFile "/data.json"
            |> Effect.Test.startViewer

-}
startViewer :
    ViewerWith (List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
    -> Program () (Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel) (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
startViewer viewerWith2 =
    Browser.application
        { init = init
        , update = update viewerWith2
        , view = view
        , subscriptions = viewerSubscriptions
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        }


{-| Msg type for a headless end to end test runner.
-}
type HeadlessMsg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    = HeadlessMsg (Result FileLoadError (List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)))


{-| Create a headless test runner.

    import Effect.Test

    main =
        Effect.Test.viewerWith
            (\image jsonData ->
                [{- End to end tests go here -}]
            )
            |> Effect.Test.addBytesFile "/test.png"
            |> Effect.Test.addBytesFile "/data.json"
            |> Effect.Test.startHeadless

-}
startHeadless :
    (Json.Encode.Value -> Cmd (HeadlessMsg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
    -> ViewerWith (List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
    -> Program () () (HeadlessMsg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
startHeadless outputResults viewerWith2 =
    Platform.worker
        { init = \_ -> ( (), Task.attempt HeadlessMsg viewerWith2.cmds )
        , update = headlessUpdate outputResults
        , subscriptions = \_ -> Sub.none
        }


headlessUpdate :
    (Json.Encode.Value -> Cmd (HeadlessMsg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel))
    -> HeadlessMsg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> ()
    -> ( (), Cmd (HeadlessMsg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel) )
headlessUpdate outputResults (HeadlessMsg result) () =
    case result of
        Ok tests ->
            let
                errors : List String
                errors =
                    List.filterMap
                        (\test ->
                            let
                                state : State toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
                                state =
                                    instructionsToState test
                            in
                            case state.testErrors of
                                [] ->
                                    Nothing

                                firstError :: _ ->
                                    " - " ++ state.testName ++ ": " ++ testErrorToString firstError |> Just
                        )
                        tests
            in
            if List.isEmpty errors then
                ( (), outputResults Json.Encode.null )

            else
                ( (), "The following tests failed:\n" ++ String.join "\n" errors |> Json.Encode.string |> outputResults )

        Err error ->
            ( ()
            , "Test setup failed: " ++ fileLoadErrorToString error |> Json.Encode.string |> outputResults
            )


viewerSubscriptions :
    Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Sub (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
viewerSubscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown decodeArrows
        , Browser.Events.onResize GotWindowSize
        ]


decodeArrows : Json.Decode.Decoder (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
decodeArrows =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                if key == "ArrowLeft" then
                    PressedArrowKey ArrowLeft |> Json.Decode.succeed

                else if key == "ArrowRight" then
                    PressedArrowKey ArrowRight |> Json.Decode.succeed

                else if key == "ArrowUp" then
                    PressedArrowKey ArrowUp |> Json.Decode.succeed

                else if key == "ArrowDown" then
                    PressedArrowKey ArrowDown |> Json.Decode.succeed

                else
                    Json.Decode.fail ""
            )


type ArrowKey
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown


{-| View your end-to-end tests in a elm reactor style app.

        main =
            viewer [{- End to end tests go here -}]

-}
viewer :
    List (Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
    -> Program () (Model toBackend frontendMsg frontendModel toFrontend backendMsg backendModel) (Msg toBackend frontendMsg frontendModel toFrontend backendMsg backendModel)
viewer tests =
    Browser.application
        { init = init
        , update = update { cmds = Task.succeed tests }
        , view = view
        , subscriptions = viewerSubscriptions
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        }


{-| Add a file containing binary data to your tests.

    import Effect.Test

    main =
        Effect.Test.viewerWith
            (\image jsonData ->
                [{- End to end tests go here -}]
            )
            |> Effect.Test.addBytesFile "/test.png"
            |> Effect.Test.addBytesFile "/data.json"
            |> Effect.Test.startViewer

-}
addBytesFile : String -> ViewerWith (Bytes -> b) -> ViewerWith b
addBytesFile file model =
    { cmds =
        Task.andThen
            (\tests ->
                Http.task
                    { method = "GET"
                    , headers = []
                    , body = Http.emptyBody
                    , url = file
                    , resolver =
                        Http.bytesResolver
                            (\response ->
                                case response of
                                    Http.BadUrl_ string ->
                                        Err { name = file, error = Http.BadUrl string |> HttpError }

                                    Http.Timeout_ ->
                                        Err { name = file, error = Http.Timeout |> HttpError }

                                    Http.NetworkError_ ->
                                        Err { name = file, error = Http.NetworkError |> HttpError }

                                    Http.BadStatus_ metadata _ ->
                                        Err { name = file, error = Http.BadStatus metadata.statusCode |> HttpError }

                                    Http.GoodStatus_ _ body ->
                                        Ok (tests body)
                            )
                    , timeout = Just 30000
                    }
            )
            model.cmds
    }


{-| Add a file containing text data to your tests.

    import Effect.Test

    main =
        Effect.Test.viewerWith
            (\text jsonData ->
                [{- End to end tests go here -}]
            )
            |> Effect.Test.addStringFile "/test.txt"
            |> Effect.Test.addStringFile "/data.json"
            |> Effect.Test.startViewer

-}
addStringFile : String -> ViewerWith (String -> b) -> ViewerWith b
addStringFile file model =
    { cmds =
        Task.andThen
            (\tests ->
                Http.task
                    { method = "GET"
                    , headers = []
                    , body = Http.emptyBody
                    , url = file
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.BadUrl_ string ->
                                        Err { name = file, error = Http.BadUrl string |> HttpError }

                                    Http.Timeout_ ->
                                        Err { name = file, error = Http.Timeout |> HttpError }

                                    Http.NetworkError_ ->
                                        Err { name = file, error = Http.NetworkError |> HttpError }

                                    Http.BadStatus_ metadata _ ->
                                        Err { name = file, error = Http.BadStatus metadata.statusCode |> HttpError }

                                    Http.GoodStatus_ _ body ->
                                        Ok (tests body)
                            )
                    , timeout = Just 30000
                    }
            )
            model.cmds
    }


{-| Add a file containing data for a `Effect.WebGL.Texture.Texture` to your tests. Right now this is performed with HTTP get requests which means you can only access files in /public (or make get requests to other websites though this isn't recommended since this API might change in the future)

    import Effect.Test

    main =
        Effect.Test.viewerWith
            (\texture ->
                [{- End to end tests go here -}]
            )
            |> Effect.Test.addTextureFile "/texture.png"
            |> Effect.Test.startViewer

-}
addTexture : String -> ViewerWith (Effect.WebGL.Texture.Texture -> b) -> ViewerWith b
addTexture file model =
    { cmds =
        Task.andThen
            (\tests ->
                WebGLFix.Texture.load file
                    |> Task.mapError (\error -> { name = file, error = TextureError error })
                    |> Task.map tests
            )
            model.cmds
    }


{-| Add a file containing data for a `Effect.WebGL.Texture.Texture` to your tests. Right now this is performed with HTTP get requests which means you can only access files in /public (or make get requests to other websites though this isn't recommended since this API might change in the future)

    import Effect.Test

    main =
        Effect.Test.viewerWith
            (\texture ->
                [{- End to end tests go here -}]
            )
            |> Effect.Test.addTextureFileWithOptions
                -- WebGL texture options go here
                "/texture.png"
            |> Effect.Test.startViewer

-}
addTextureWithOptions : Effect.WebGL.Texture.Options -> String -> ViewerWith (Effect.WebGL.Texture.Texture -> b) -> ViewerWith b
addTextureWithOptions options file model =
    let
        convertWrap : Effect.Internal.Wrap -> WebGLFix.Texture.Wrap
        convertWrap wrap =
            case wrap of
                Effect.Internal.Repeat ->
                    WebGLFix.Texture.repeat

                Effect.Internal.ClampToEdge ->
                    WebGLFix.Texture.clampToEdge

                Effect.Internal.MirroredRepeat ->
                    WebGLFix.Texture.mirroredRepeat
    in
    { cmds =
        Task.andThen
            (\tests ->
                WebGLFix.Texture.loadWith
                    { magnify =
                        case options.magnify of
                            Effect.Internal.Linear ->
                                WebGLFix.Texture.linear

                            _ ->
                                WebGLFix.Texture.nearest
                    , minify =
                        case options.minify of
                            Effect.Internal.Linear ->
                                WebGLFix.Texture.linear

                            Effect.Internal.Nearest ->
                                WebGLFix.Texture.nearest

                            Effect.Internal.NearestMipmapNearest ->
                                WebGLFix.Texture.nearestMipmapNearest

                            Effect.Internal.LinearMipmapNearest ->
                                WebGLFix.Texture.linearMipmapNearest

                            Effect.Internal.NearestMipmapLinear ->
                                WebGLFix.Texture.nearestMipmapLinear

                            Effect.Internal.LinearMipmapLinear ->
                                WebGLFix.Texture.linearMipmapLinear
                    , horizontalWrap = convertWrap options.horizontalWrap
                    , verticalWrap = convertWrap options.verticalWrap
                    , flipY = options.flipY
                    , premultiplyAlpha = options.premultiplyAlpha
                    }
                    file
                    |> Task.mapError (\error -> { name = file, error = TextureError error })
                    |> Task.map tests
            )
            model.cmds
    }
