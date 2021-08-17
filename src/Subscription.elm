module Subscription exposing (Subscription, batch, fromJs, none, onResize, timeEvery)

import Duration exposing (Duration)
import Effect.Internal
import Json.Decode
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
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
