module Tests exposing (..)

import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Subscription as Subscription
import Effect.Test
import Effect.Time as Time
import Html
import Test exposing (Test)
import Url


frontendApp : Effect.Test.FrontendApp {} {} {} {}
frontendApp =
    { init = \_ _ -> ( {}, Command.none )
    , onUrlRequest = \_ -> {}
    , onUrlChange = \_ -> {}
    , update = \_ _ -> ( {}, Command.none )
    , updateFromBackend = \_ _ -> ( {}, Command.none )
    , view = \_ -> { title = "Hi", body = [ Html.text "Hi" ] }
    , subscriptions = \_ -> Subscription.none
    }


backendApp : Effect.Test.BackendApp {} {} {} {}
backendApp =
    { init = ( {}, Command.none )
    , update = \_ _ -> ( {}, Command.none )
    , updateFromFrontend = \_ _ _ _ -> ( {}, Command.none )
    , subscriptions = \_ -> Subscription.none
    }


unsafeUrl =
    case Url.fromString "my-test.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


config : Effect.Test.Config {} {} {} {} {} {}
config =
    { frontendApp = frontendApp
    , backendApp = backendApp
    , handleHttpRequest = always NetworkError_
    , handlePortToJs = always Nothing
    , handleFileRequest = always Nothing
    , domain = unsafeUrl
    , deployTime = Time.millisToPosix 0
    }


programTests =
    [ Effect.Test.start config "A test"
    ]


tests : Test
tests =
    List.map Effect.Test.toTest programTests |> Test.describe "Tests"


main : Program () (Effect.Test.Model {}) Effect.Test.Msg
main =
    Effect.Test.viewer programTests
