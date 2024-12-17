module Backend exposing (app, app_, init)

import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription
import Lamdera
import Set exposing (Set)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        app_


app_ =
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }


init : ( Model, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { counter = 0 }, Command.none )


update : BackendMsg -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( model, Effect.Lamdera.sendToFrontend clientId <| CounterNewValue model.counter clientId )

        Noop ->
            ( model, Command.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CounterIncremented ->
            let
                newCounter =
                    model.counter + 1
            in
            ( { model | counter = newCounter }
            , Effect.Lamdera.broadcast (CounterNewValue newCounter clientId)
            )

        CounterDecremented ->
            let
                newCounter =
                    model.counter - 1
            in
            ( { model | counter = newCounter }
            , Effect.Lamdera.broadcast (CounterNewValue newCounter clientId)
            )


subscriptions model =
    Subscription.batch
        [ Effect.Lamdera.onConnect ClientConnected
        ]
