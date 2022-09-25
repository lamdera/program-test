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
                    { model | timeEveryEvents = model.timeEveryEvents ++ [ time ] }
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
        , Effect.Test.start timeEveryConfig "simulateTime test 0"
            |> checkTimeEveryModel 0 []
            |> simulateMillis 100
            |> checkTimeEveryModel 1 []
            |> checkTime 100
            |> Effect.Test.toTest
        , Effect.Test.start timeEveryConfig "simulateTime test 1"
            |> checkTimeEveryModel 0 []
            |> simulateMillis 1000
            |> checkTimeEveryModel 1 [ Time.millisToPosix 1000 ]
            |> checkTime 1000
            |> Effect.Test.toTest
        , Effect.Test.start timeEveryConfig "simulateTime test 2"
            |> checkTimeEveryModel 0 []
            |> simulateMillis 200
            |> checkTimeEveryModel 1 []
            |> simulateMillis 800
            |> checkTimeEveryModel 2 [ Time.millisToPosix 1000 ]
            |> checkTime 1000
            |> Effect.Test.toTest
        , Effect.Test.start timeEveryConfig "simulateTime test 3"
            |> checkTimeEveryModel 0 []
            |> simulateMillis 1200
            |> checkTimeEveryModel 1 [ Time.millisToPosix 1000 ]
            |> checkTime 1200
            |> Effect.Test.toTest
        ]


checkTimeEveryModel index expected =
    Effect.Test.checkBackend
        (\model ->
            if model.timeEveryEvents == expected then
                Ok ()

            else
                Err ("Check " ++ String.fromInt index ++ " failed: " ++ Debug.toString model)
        )


simulateMillis :
    Float
    -> Effect.Test.Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Effect.Test.Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
simulateMillis millis instructions =
    Effect.Test.simulateTime (Duration.milliseconds millis) instructions


checkTime :
    Int
    -> Effect.Test.Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> Effect.Test.Instructions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
checkTime expectedMillis =
    Effect.Test.checkState
        (\state ->
            if Time.millisToPosix expectedMillis == state.currentTime then
                Ok ()

            else
                Err ("Incorrect time: " ++ Debug.toString state.currentTime)
        )


main : Program () (Effect.Test.Model {}) Effect.Test.Msg
main =
    Effect.Test.viewer [ simpleTest ]
