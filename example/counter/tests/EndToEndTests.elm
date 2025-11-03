module Tests exposing (main)

import Backend
import Effect.Browser.Dom as Dom
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Test exposing (FileUpload(..), HttpResponse(..), MultipleFilesUpload(..))
import Effect.Time
import Frontend
import Html
import Test.Html.Query
import Test.Html.Selector
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


unsafeUrl : Url
unsafeUrl =
    case Url.fromString "https://chat-app.lamdera.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


config : Effect.Test.Config ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
config =
    { frontendApp = Frontend.app_
    , backendApp = Backend.app_
    , handleHttpRequest = always NetworkErrorResponse
    , handlePortToJs = always Nothing
    , handleFileUpload = always UnhandledFileUpload
    , handleMultipleFilesUpload = always UnhandledMultiFileUpload
    , domain = unsafeUrl
    }


tests : List (Effect.Test.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests =
    [ Effect.Test.start
        "Clients stay in sync"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "sessionId0")
            "/"
            { width = 800, height = 600 }
            (\client1 ->
                [ client1.click 100 (Dom.id "plusOne")
                , client1.click 100 (Dom.id "plusOne")
                , client1.click 100 (Dom.id "plusOne")
                , client1.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.exactText "3" ])
                , Effect.Test.connectFrontend
                    100
                    (Effect.Lamdera.sessionIdFromString "sessionId1")
                    "/"
                    { width = 800, height = 600 }
                    (\client2 ->
                        [ client2.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.exactText "3" ])
                        , client2.click 100 (Dom.id "minusOne")
                        , client1.checkView 100 (Test.Html.Query.has [ Test.Html.Selector.exactText "2" ])
                        ]
                    )
                ]
            )
        ]
    ]


main : Program () (Effect.Test.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (Effect.Test.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    Effect.Test.viewer tests
