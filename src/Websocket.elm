effect module Websocket where { command = MyCmd, subscription = MySub } exposing (Connection(..), SendError(..), close, createHandle, listen, sendString, CloseEventCode(..))

{-|

@docs Connection, SendError, close, createHandle, listen, sendString, CloseEventCode

-}

import Dict exposing (Dict)
import Elm.Kernel.LamderaWebsocket
import Process
import Task exposing (Task)


{-| A websocket connection
-}
type Connection
    = Connection String String


{-| Create a websocket handle that you can then open by calling listen or sendString.
-}
createHandle : String -> Task x Connection
createHandle url =
    Elm.Kernel.LamderaWebsocket.createHandle () url


{-| Errors that might happen when sending data.
-}
type SendError
    = ConnectionClosed


{-| Here are some possible reasons that your websocket connection closed.
-}
type CloseEventCode
    = NormalClosure
    | GoingAway
    | ProtocolError
    | UnsupportedData
    | NoStatusReceived
    | AbnormalClosure
    | InvalidFramePayloadData
    | PolicyViolation
    | MessageTooBig
    | MissingExtension
    | InternalError
    | ServiceRestart
    | TryAgainLater
    | BadGateway
    | TlsHandshake
    | UnknownCode Int


decodeCloseEventCode : Int -> CloseEventCode
decodeCloseEventCode code =
    case code of
        1000 ->
            NormalClosure

        1001 ->
            GoingAway

        1002 ->
            ProtocolError

        1003 ->
            UnsupportedData

        1005 ->
            NoStatusReceived

        1006 ->
            AbnormalClosure

        1007 ->
            InvalidFramePayloadData

        1008 ->
            PolicyViolation

        1009 ->
            MessageTooBig

        1010 ->
            MissingExtension

        1011 ->
            InternalError

        1012 ->
            ServiceRestart

        1013 ->
            TryAgainLater

        1014 ->
            BadGateway

        1015 ->
            TlsHandshake

        _ ->
            UnknownCode code


connectionClosed : SendError
connectionClosed =
    ConnectionClosed


{-| Send a string
-}
sendString : Connection -> String -> Task SendError ()
sendString connection_ data =
    Elm.Kernel.LamderaWebsocket.sendString () connection_ data
        |> Task.map (\_ -> ())


{-| Close the websocket connection
-}
close : Connection -> Task x ()
close connection_ =
    Elm.Kernel.LamderaWebsocket.close () connection_
        |> Task.map (\_ -> ())


{-| Listen for incoming messages through a websocket connection. You'll also get notified if the connection closes.
-}
listen : Connection -> (String -> msg) -> ({ code : CloseEventCode, reason : String } -> msg) -> Sub msg
listen connection_ onData onClose =
    subscription (Listen connection_ onData onClose)


init : Task Never (State msg)
init =
    Task.succeed { connections = Dict.empty }


type alias State msg =
    { connections :
        Dict
            String
            ( Process.Id
            , List
                { onData : String -> msg
                , onClose : { code : CloseEventCode, reason : String } -> msg
                }
            )
    }


type alias SelfMsg =
    ( Connection, MyEvent )


type MyEvent
    = DataEvent String
    | ClosedEvent { code : CloseEventCode, reason : String }


dataEvent : String -> MyEvent
dataEvent =
    DataEvent


closedEvent : Int -> String -> MyEvent
closedEvent code reason =
    ClosedEvent { code = decodeCloseEventCode code, reason = reason }


connection : String -> String -> Connection
connection =
    Connection


onSelfMsg : Platform.Router msg SelfMsg -> SelfMsg -> State msg -> Task Never (State msg)
onSelfMsg router ( Connection connectionId _, event ) state =
    case Dict.get connectionId state.connections of
        Just ( _, msgs ) ->
            case event of
                ClosedEvent data ->
                    List.map (\{ onClose } -> Platform.sendToApp router (onClose data)) msgs
                        |> Task.sequence
                        |> Task.map (\_ -> state)

                DataEvent data ->
                    List.map (\{ onData } -> Platform.sendToApp router (onData data)) msgs
                        |> Task.sequence
                        |> Task.map (\_ -> state)

        Nothing ->
            Task.succeed state


type MySub msg
    = Listen Connection (String -> msg) ({ code : CloseEventCode, reason : String } -> msg)


type MyCmd msg
    = SentData Connection String
    | OpenConnection String (Connection -> msg)
    | CloseConnection Connection


onEffects :
    Platform.Router msg SelfMsg
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router _ subs state =
    let
        handleSubs remainingSubs newDict =
            case remainingSubs of
                [] ->
                    Task.succeed newDict

                (Listen ((Connection connectionId _) as connection_) onData onClose) :: rest ->
                    case Dict.get connectionId newDict of
                        Just ( pid, msgs ) ->
                            handleSubs
                                rest
                                (Dict.insert
                                    connectionId
                                    ( pid, { onData = onData, onClose = onClose } :: msgs )
                                    newDict
                                )

                        Nothing ->
                            Process.spawn (Elm.Kernel.LamderaWebsocket.listen router connection_)
                                |> Task.andThen
                                    (\pid ->
                                        handleSubs
                                            rest
                                            (Dict.insert
                                                connectionId
                                                ( pid, [ { onData = onData, onClose = onClose } ] )
                                                newDict
                                            )
                                    )
    in
    handleSubs subs (Dict.map (\_ ( pid, _ ) -> ( pid, [] )) state.connections)
        |> Task.map (\dict -> { state | connections = dict })


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        Listen url onData onClose ->
            Listen url (\a -> onData a |> func) (\a -> onClose a |> func)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        SentData connection_ data ->
            SentData connection_ data

        OpenConnection url onOpen ->
            OpenConnection url (\a -> onOpen a |> f)

        CloseConnection connection_ ->
            CloseConnection connection_
