module Subscription exposing (Subscription, batch, fromJs, map, none, onResize, timeEvery)

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


onResize : (Int -> Int -> msg) -> Subscription FrontendOnly msg
onResize =
    Effect.Internal.OnResize


fromJs : String -> ((Json.Decode.Value -> msg) -> Sub msg) -> (Json.Decode.Value -> msg) -> Subscription FrontendOnly msg
fromJs portName portFunction msg =
    Effect.Internal.SubPort portName (portFunction msg) msg


map : (a -> b) -> Subscription restriction a -> Subscription restriction b
map mapFunc subscription =
    case subscription of
        Effect.Internal.SubBatch subscriptions ->
            List.map (map mapFunc) subscriptions |> Effect.Internal.SubBatch

        Effect.Internal.TimeEvery duration msg ->
            Effect.Internal.TimeEvery duration (msg >> mapFunc)

        Effect.Internal.OnAnimationFrame msg ->
            Effect.Internal.OnAnimationFrame (msg >> mapFunc)

        Effect.Internal.OnAnimationFrameDelta msg ->
            Effect.Internal.OnAnimationFrameDelta (msg >> mapFunc)

        Effect.Internal.OnKeyPress decoder ->
            Effect.Internal.OnKeyPress (Json.Decode.map mapFunc decoder)

        Effect.Internal.OnKeyDown decoder ->
            Effect.Internal.OnKeyPress (Json.Decode.map mapFunc decoder)

        Effect.Internal.OnKeyUp decoder ->
            Effect.Internal.OnKeyUp (Json.Decode.map mapFunc decoder)

        Effect.Internal.OnClick decoder ->
            Effect.Internal.OnClick (Json.Decode.map mapFunc decoder)

        Effect.Internal.OnMouseMove decoder ->
            Effect.Internal.OnMouseMove (Json.Decode.map mapFunc decoder)

        Effect.Internal.OnMouseDown decoder ->
            Effect.Internal.OnMouseDown (Json.Decode.map mapFunc decoder)

        Effect.Internal.OnMouseUp decoder ->
            Effect.Internal.OnMouseUp (Json.Decode.map mapFunc decoder)

        Effect.Internal.OnVisibilityChange msg ->
            Effect.Internal.OnVisibilityChange (msg >> mapFunc)

        Effect.Internal.OnResize msg ->
            Effect.Internal.OnResize (\w h -> msg w h |> mapFunc)

        Effect.Internal.SubPort portName sub msg ->
            Effect.Internal.SubPort portName (Sub.map mapFunc sub) (msg >> mapFunc)

        Effect.Internal.SubNone ->
            Effect.Internal.SubNone

        Effect.Internal.OnConnect msg ->
            Effect.Internal.OnConnect (\sessionId clientId -> msg sessionId clientId |> mapFunc)

        Effect.Internal.OnDisconnect msg ->
            Effect.Internal.OnDisconnect (\sessionId clientId -> msg sessionId clientId |> mapFunc)
