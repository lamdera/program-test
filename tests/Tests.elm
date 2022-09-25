module Tests exposing (..)

import Duration
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http exposing (Response(..))
import Effect.Subscription as Subscription
import Effect.Test
import Effect.Time as Time
import Expect
import Html
import Test exposing (Test)
import Url


simpleFrontend : Effect.Test.FrontendApp {} {} {} {}
simpleFrontend =
    { init = \_ _ -> ( {}, Command.none )
    , onUrlRequest = \_ -> {}
    , onUrlChange = \_ -> {}
    , update = \_ _ -> ( {}, Command.none )
    , updateFromBackend = \_ _ -> ( {}, Command.none )
    , view = \_ -> { title = "Hi", body = [ Html.text "Hi" ] }
    , subscriptions = \_ -> Subscription.none
    }


simpleBackend : Effect.Test.BackendApp {} {} {} {}
simpleBackend =
    { init = ( {}, Command.none )
    , update = \_ _ -> ( {}, Command.none )
    , updateFromFrontend = \_ _ _ _ -> ( {}, Command.none )
    , subscriptions = \_ -> Subscription.none
    }


unsafeUrl =
    case Url.fromString "https://my-test.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


simpleConfig : Effect.Test.Config {} {} {} {} {} {}
simpleConfig =
    { frontendApp = simpleFrontend
    , backendApp = simpleBackend
    , handleHttpRequest = always NetworkError_
    , handlePortToJs = always Nothing
    , handleFileRequest = always Nothing
    , domain = unsafeUrl
    , deployTime = Time.millisToPosix 0
    }


type Msg
    = GotTime Time.Posix


type alias Model =
    { timeEveryEvents : List Time.Posix }


timeEveryBackend : Effect.Test.BackendApp {} {} Msg Model
timeEveryBackend =
    { init = ( { timeEveryEvents = [] }, Command.none )
    , update =
        \msg model ->
            ( case msg of
                GotTime time ->
                    { model | timeEveryEvents = time :: model.timeEveryEvents }
            , Command.none
            )
    , updateFromFrontend = \_ _ _ model -> ( model, Command.none )
    , subscriptions = \_ -> Time.every Duration.second GotTime
    }


timeEveryConfig : Effect.Test.Config {} {} {} {} Msg Model
timeEveryConfig =
    { frontendApp = simpleFrontend
    , backendApp = timeEveryBackend
    , handleHttpRequest = always NetworkError_
    , handlePortToJs = always Nothing
    , handleFileRequest = always Nothing
    , domain = unsafeUrl
    , deployTime = Time.millisToPosix 0
    }


simpleTest : Effect.Test.Instructions {} {} {} {} {} {}
simpleTest =
    Effect.Test.start simpleConfig "A test"


tests : Test
tests =
    Test.describe "Tests"
        [ Effect.Test.start simpleConfig "A test" |> Effect.Test.toTest
        , Effect.Test.start timeEveryConfig "simulateTime test"
            |> Effect.Test.checkBackend
                (\model ->
                    if model.timeEveryEvents == [] then
                        Ok ()

                    else
                        Err ("Check 1 failed: " ++ Debug.toString model)
                )
            |> Effect.Test.simulateTime (Duration.milliseconds 100)
            |> Effect.Test.checkBackend
                (\model ->
                    if model.timeEveryEvents == [ Time.millisToPosix 1000 ] then
                        Ok ()

                    else
                        Err ("Check 2 failed: " ++ Debug.toString model)
                )
            |> Effect.Test.toTest
        ]


main : Program () (Effect.Test.Model {}) Effect.Test.Msg
main =
    Effect.Test.viewer [ simpleTest ]
