module Tests exposing (main)

import Dict
import Effect.Browser.Dom as Dom
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Test exposing (FileUpload(..), HttpResponse(..), MultipleFilesUpload(..))
import Effect.Time
import Html
import Json.Encode
import Main
import Test.Html.Query
import Test.Html.Selector
import Url exposing (Url)


unsafeUrl : Url
unsafeUrl =
    case Url.fromString "https://chat-app.lamdera.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


config : Effect.Test.Config Never Main.Msg Main.Model Never Never ()
config =
    Effect.Test.configForElement
        { flags = ()
        , frontendApp =
            { init = Main.app.init
            , update = Main.app.update
            , subscriptions = Main.app.subscriptions
            , view = Main.app.view
            }
        , noOpMsg = Main.MorePlease
        , handleHttpRequest = handleHttpRequest
        , handlePortToJs = always Nothing
        , handleFileUpload = always UnhandledFileUpload
        , handleMultipleFilesUpload = always UnhandledMultiFileUpload
        , domain = unsafeUrl
        }


handleHttpRequest :
    { data : Effect.Test.Data frontendModel (), currentRequest : Effect.Test.HttpRequest }
    -> HttpResponse
handleHttpRequest { currentRequest, data } =
    if currentRequest.url == "https://elm-lang.org/api/random-quotes" then
        JsonHttpResponse
            { url = "https://elm-lang.org/api/random-quotes"
            , statusCode = 200
            , statusText = "Ok"
            , headers = Dict.empty
            }
            (if List.isEmpty data.httpRequests then
                Json.Encode.object
                    [ ( "quote", Json.Encode.string "You're reading a quote from a fake http request!" )
                    , ( "source", Json.Encode.string "EndToEndTests.handleHttpRequest" )
                    , ( "author", Json.Encode.string "Martin" )
                    , ( "year", Json.Encode.int 2025 )
                    ]

             else
                Json.Encode.object
                    [ ( "quote", Json.Encode.string "And here's another quote I made up on the spot to fill out the blank space and let you see that you can trigger different http responses for repeated requests to the same url." )
                    , ( "source", Json.Encode.string "EndToEndTests.handleHttpRequest" )
                    , ( "author", Json.Encode.string "Martin" )
                    , ( "year", Json.Encode.int 2025 )
                    ]
            )

    else
        UnhandledHttpRequest


tests : List (Effect.Test.EndToEndTest Never Main.Msg Main.Model Never Never ())
tests =
    [ Effect.Test.start
        "View quotes"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "sessionId0")
            "/"
            { width = 800, height = 600 }
            (\client1 ->
                [ client1.checkView 0 (Test.Html.Query.has [ Test.Html.Selector.text "Loading..." ])
                , client1.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.text "You're reading a quote from a fake http request!" ])
                , client1.click 1000 (Dom.id "more-please")
                , client1.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.text "And here's another quote" ])
                ]
            )
        ]
    ]


main : Program () (Effect.Test.Model Never Main.Msg Main.Model Never Never ()) (Effect.Test.Msg Never Main.Msg Main.Model Never Never ())
main =
    Effect.Test.viewer tests
