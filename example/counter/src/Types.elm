module Types exposing (..)

import Effect.Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)


type alias BackendModel =
    { counter : Int
    }


type alias FrontendModel =
    { counter : Int
    , clientId : Maybe ClientId
    }


type FrontendMsg
    = Increment
    | Decrement
    | FNoop


type ToBackend
    = CounterIncremented
    | CounterDecremented


type BackendMsg
    = ClientConnected SessionId ClientId
    | Noop


type ToFrontend
    = CounterNewValue Int ClientId
