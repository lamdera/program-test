module Backend exposing (app)

import Effect.Command
import Effect.Lamdera
import Effect.Subscription
import Lamdera
import Set
import Types


type alias Model =
    Types.BackendModel


type alias Msg =
    Types.BackendMsg


type alias Cmd_Msg =
    Effect.Command.Command Effect.Command.BackendOnly Types.ToFrontend Msg


type alias Sub_Msg =
    Effect.Subscription.Subscription Effect.Command.BackendOnly Msg


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        { init = ( Set.empty, Effect.Command.none )
        , update = update
        , updateFromFrontend = \_ _ _ model -> ( model, Effect.Command.none )
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub_Msg
subscriptions _ =
    Effect.Subscription.batch
        [ Effect.Lamdera.onConnect Types.ClientConnected
        , Effect.Lamdera.onDisconnect Types.ClientDisconnected
        ]


update : Msg -> Model -> ( Model, Cmd_Msg )
update msg model =
    case msg of
        Types.ClientConnected _ clientId ->
            broadcastConnectionCount (Set.insert (Effect.Lamdera.clientIdToString clientId) model)

        Types.ClientDisconnected _ clientId ->
            broadcastConnectionCount (Set.remove (Effect.Lamdera.clientIdToString clientId) model)


broadcastConnectionCount : Model -> ( Model, Cmd_Msg )
broadcastConnectionCount model =
    ( model, Effect.Lamdera.broadcast (Types.ConnectionCount (Set.size model)) )
