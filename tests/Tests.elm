module Tests exposing (..)

import Duration exposing (Duration)
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


type FrontendMsg
    = GotTimeFrontend Time.Posix
    | NoOpFrontend


type BackendMsg
    = GotTimeBackend Time.Posix


type alias Model =
    { timeEveryEvents : List Time.Posix }


timeEveryFrontend : List Duration -> Effect.Test.FrontendApp {} FrontendMsg Model {}
timeEveryFrontend durations =
    { init = \_ _ -> ( { timeEveryEvents = [] }, Command.none )
    , onUrlRequest = \_ -> NoOpFrontend
    , onUrlChange = \_ -> NoOpFrontend
    , update =
        \msg model ->
            ( case msg of
                GotTimeFrontend time ->
                    { model | timeEveryEvents = model.timeEveryEvents ++ [ time ] }

                NoOpFrontend ->
                    model
            , Command.none
            )
    , updateFromBackend = \_ model -> ( model, Command.none )
    , view = \_ -> { title = "Hi", body = [ Html.text "Hi" ] }
    , subscriptions = \_ -> List.map (\duration -> Time.every duration GotTimeFrontend) durations |> Subscription.batch
    }


timeEveryBackend : List Duration -> Effect.Test.BackendApp {} {} BackendMsg Model
timeEveryBackend durations =
    { init = ( { timeEveryEvents = [] }, Command.none )
    , update =
        \msg model ->
            ( case msg of
                GotTimeBackend time ->
                    { model | timeEveryEvents = model.timeEveryEvents ++ [ time ] }
            , Command.none
            )
    , updateFromFrontend = \_ _ _ model -> ( model, Command.none )
    , subscriptions = \_ -> List.map (\duration -> Time.every duration GotTimeBackend) durations |> Subscription.batch
    }


timeEveryConfig : List Duration -> List Duration -> Effect.Test.Config {} FrontendMsg Model {} BackendMsg Model
timeEveryConfig frontendDurations backendDurations =
    { frontendApp = timeEveryFrontend frontendDurations
    , backendApp = timeEveryBackend backendDurations
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
        , Effect.Test.start (timeEveryConfig [] [ Duration.second ]) "simulateTime test 0"
            |> checkTimeEveryModel 0 []
            |> simulateMillis 100
            |> checkTimeEveryModel 1 []
            |> checkTime 100
            |> Effect.Test.toTest
        , Effect.Test.start (timeEveryConfig [] [ Duration.second ]) "simulateTime test 1"
            |> simulateMillis 1000
            |> checkTimeEveryModel 1 [ Time.millisToPosix 1000 ]
            |> checkTime 1000
            |> Effect.Test.toTest
        , Effect.Test.start (timeEveryConfig [] [ Duration.second ]) "simulateTime test 2"
            |> simulateMillis 200
            |> checkTimeEveryModel 1 []
            |> simulateMillis 800
            |> checkTimeEveryModel 2 [ Time.millisToPosix 1000 ]
            |> checkTime 1000
            |> Effect.Test.toTest
        , Effect.Test.start (timeEveryConfig [] [ Duration.second ]) "simulateTime test 3"
            |> simulateMillis 1200
            |> checkTimeEveryModel 1 [ Time.millisToPosix 1000 ]
            |> checkTime 1200
            |> Effect.Test.toTest
        , Effect.Test.start (timeEveryConfig [] [ Duration.second ]) "simulateTime test 4"
            |> simulateMillis 2000
            |> checkTimeEveryModel 1 [ Time.millisToPosix 1000, Time.millisToPosix 2000 ]
            |> simulateMillis 1
            |> checkTimeEveryModel 2 [ Time.millisToPosix 1000, Time.millisToPosix 2000 ]
            |> checkTime 2001
            |> Effect.Test.toTest
        , Effect.Test.start (timeEveryConfig [] [ Duration.second, Duration.seconds 2 ]) "simulateTime test 5"
            |> checkTimeEveryModel 0 []
            |> simulateMillis 100
            |> checkTimeEveryModel 1 []
            |> checkTime 100
            |> Effect.Test.toTest
        , Effect.Test.start (timeEveryConfig [] [ Duration.second, Duration.seconds 2 ]) "simulateTime test 6"
            |> simulateMillis 2000
            |> checkTimeEveryModel 1 [ Time.millisToPosix 1000, Time.millisToPosix 2000, Time.millisToPosix 2000 ]
            |> checkTime 2000
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
