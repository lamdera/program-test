module Types exposing (..)

import Effect.Lamdera
import Set exposing (Set)


type alias FrontendMsg =
    ()


type alias FrontendModel =
    Maybe Int


type ToFrontend
    = ConnectionCount Int


type alias ToBackend =
    ()


type alias BackendModel =
    Set String


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
