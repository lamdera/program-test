module Subscription exposing (Subscription, batch, fromJs, none, onConnect, onDisconnect, onResize, timeEvery, toSub)

import Browser.Events
import Duration exposing (Duration)
import Effect.Internal
import Json.Decode
import Lamdera
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import TestId exposing (ClientId, SessionId)
import Time


type alias FrontendOnly =
    Effect.Internal.FrontendOnly


type alias BackendOnly =
    Effect.Internal.BackendOnly


type alias Subscription restriction msg =
    Effect.Internal.Subscription restriction msg


batch : List (Subscription restriction msg) -> Subscription restriction msg
batch =
    Effect.Internal.SubBatch


none : Subscription restriction msg
none =
    Effect.Internal.SubNone


timeEvery : Duration -> (Time.Posix -> msg) -> Subscription restriction msg
timeEvery =
    Effect.Internal.TimeEvery


onResize : (Quantity Int Pixels -> Quantity Int Pixels -> msg) -> Subscription FrontendOnly msg
onResize =
    Effect.Internal.OnResize


fromJs : String -> ((Json.Decode.Value -> msg) -> Sub msg) -> (Json.Decode.Value -> msg) -> Subscription FrontendOnly msg
fromJs =
    Effect.Internal.SubPort


onConnect : (SessionId -> ClientId -> msg) -> Subscription BackendOnly msg
onConnect =
    Effect.Internal.OnConnect


onDisconnect : (SessionId -> ClientId -> msg) -> Subscription BackendOnly msg
onDisconnect =
    Effect.Internal.OnDisconnect



--map : (a -> b) -> FrontendSub a -> FrontendSub b
--map mapFunc subscription =
--    case subscription of
--        Batch subscriptions ->
--            List.map (map mapFunc) subscriptions |> Batch
--
--        TimeEvery duration msg ->
--            TimeEvery duration (msg >> mapFunc)
--
--        OnResize msg ->
--            OnResize (\w h -> msg w h |> mapFunc)
--
--        Port portName portFunction msg ->
--            let
--                portFunction_ : (Json.Decode.Value -> b) -> Sub b
--                portFunction_ msg_ =
--                    portFunction msg_ |> Sub.map mapFunc
--            in
--            Port portName portFunction_ (msg >> mapFunc)


toSub : Subscription restriction msg -> Sub msg
toSub sub =
    case sub of
        Effect.Internal.SubBatch subs ->
            List.map toSub subs |> Sub.batch

        Effect.Internal.SubNone ->
            Sub.none

        Effect.Internal.TimeEvery duration msg ->
            Time.every (Duration.inMilliseconds duration) msg

        Effect.Internal.OnResize msg ->
            Browser.Events.onResize (\w h -> msg (Pixels.pixels w) (Pixels.pixels h))

        Effect.Internal.SubPort _ portFunction msg ->
            portFunction msg

        Effect.Internal.OnConnect msg ->
            Lamdera.onConnect
                (\sessionId clientId -> msg (TestId.sessionIdFromString sessionId) (TestId.clientIdFromString clientId))

        Effect.Internal.OnDisconnect msg ->
            Lamdera.onDisconnect
                (\sessionId clientId -> msg (TestId.sessionIdFromString sessionId) (TestId.clientIdFromString clientId))
