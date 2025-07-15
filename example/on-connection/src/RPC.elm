module RPC exposing (..)

import Http
import Json.Encode as Json
import Lamdera exposing (SessionId)
import LamderaRPC exposing (..)
import Types exposing (BackendModel, BackendMsg)


helloWorld : SessionId -> BackendModel -> LamderaRPC.Headers -> Json.Value -> ( Result Http.Error Json.Value, BackendModel, Cmd msg )
helloWorld sessionId model headers value =
    let
        response =
            [ ( "greeting", Json.string "Hello world!" ) ]
    in
    ( Ok <| Json.object response, model, Cmd.none )


notFound : SessionId -> BackendModel -> LamderaRPC.Headers -> Json.Value -> ( Result Http.Error Json.Value, BackendModel, Cmd msg )
notFound sessionId model headers value =
    let
        response =
            [ ( "greeting", Json.string "Hello world!" ) ]
    in
    ( Err Http.NetworkError, model, Cmd.none )


lamdera_handleEndpoints : Json.Value -> HttpRequest -> BackendModel -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints reqRaw req model =
    case req.endpoint of
        "hello-world" ->
            LamderaRPC.handleEndpointJson helloWorld req model

        _ ->
            LamderaRPC.handleEndpointJson notFound req model
