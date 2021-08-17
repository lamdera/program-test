module TestInternal exposing
    ( Effect(..)
    , File(..)
    , HttpBody(..)
    , HttpRequest
    , NavigationKey(..)
    , SimulatedTask(..)
    , Subscription(..)
    )

import Browser.Dom
import Browser.Navigation
import Duration exposing (Duration)
import File
import File.Select
import Http
import Json.Decode
import Json.Encode
import Lamdera
import Pixels exposing (Pixels)
import Process
import Quantity exposing (Quantity)
import Task
import TestId exposing (ClientId, SessionId)
import Time


type Subscription restriction msg
    = SubBatch (List (Subscription restriction msg))
    | SubNone
    | TimeEvery Duration (Time.Posix -> msg)
    | OnResize (Quantity Int Pixels -> Quantity Int Pixels -> msg)
    | SubPort String ((Json.Decode.Value -> msg) -> Sub msg) (Json.Decode.Value -> msg)
    | OnConnect (SessionId -> ClientId -> msg)
    | OnDisconnect (SessionId -> ClientId -> msg)


type Effect restriction toMsg msg
    = Batch (List (Effect restriction toMsg msg))
    | None
    | SendToBackend toMsg
    | NavigationPushUrl NavigationKey String
    | NavigationReplaceUrl NavigationKey String
    | NavigationLoad String
    | SelectFile (List String) (File -> msg)
    | FileToUrl (String -> msg) File
    | Task (SimulatedTask restriction msg msg)
    | Port String (Json.Encode.Value -> Cmd msg) Json.Encode.Value
    | SendToFrontend ClientId toMsg


type SimulatedTask restriction x a
    = Succeed a
    | Fail x
    | HttpTask (HttpRequest restriction x a)
    | SleepTask Duration (() -> SimulatedTask restriction x a)
    | GetTime (Time.Posix -> SimulatedTask restriction x a)
    | GetTimeZone (Time.Zone -> SimulatedTask restriction x a)
    | GetTimeZoneName (Time.ZoneName -> SimulatedTask restriction x a)
    | GetViewport (Browser.Dom.Viewport -> SimulatedTask restriction x a)
    | SetViewport (Quantity Float Pixels) (Quantity Float Pixels) (() -> SimulatedTask restriction x a)
    | GetElement (Result Browser.Dom.Error Browser.Dom.Element -> SimulatedTask restriction x a) String


type NavigationKey
    = RealNavigationKey Browser.Navigation.Key
    | MockNavigationKey


type File
    = RealFile File.File
    | MockFile { name : String, content : String }


type alias HttpRequest restriction x a =
    { method : String
    , url : String
    , body : HttpBody
    , headers : List ( String, String )
    , onRequestComplete : Http.Response String -> SimulatedTask restriction x a
    , timeout : Maybe Duration
    }


type HttpBody
    = EmptyBody
    | StringBody
        { contentType : String
        , content : String
        }
    | JsonBody Json.Encode.Value


toCmd : Effect restriction toBackend frontendMsg -> Cmd frontendMsg
toCmd effect =
    case effect of
        Batch effects ->
            List.map toCmd effects |> Cmd.batch

        None ->
            Cmd.none

        SendToBackend toBackend ->
            Lamdera.sendToBackend toBackend

        NavigationPushUrl navigationKey string ->
            case navigationKey of
                RealNavigationKey key ->
                    Browser.Navigation.pushUrl key string

                MockNavigationKey ->
                    Cmd.none

        NavigationReplaceUrl navigationKey string ->
            case navigationKey of
                RealNavigationKey key ->
                    Browser.Navigation.replaceUrl key string

                MockNavigationKey ->
                    Cmd.none

        NavigationLoad url ->
            Browser.Navigation.load url

        SelectFile mimeTypes msg ->
            File.Select.file mimeTypes (RealFile >> msg)

        FileToUrl msg file ->
            case file of
                RealFile realFile ->
                    File.toUrl realFile |> Task.perform msg

                MockFile _ ->
                    Cmd.none

        Task simulatedTask ->
            toTask simulatedTask
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok ok ->
                                ok

                            Err err ->
                                err
                    )

        Port _ portFunction value ->
            portFunction value

        SendToFrontend clientId toFrontend ->
            Lamdera.sendToFrontend (TestId.clientIdToString clientId) toFrontend


toTask : SimulatedTask restriction x b -> Task.Task x b
toTask simulatedTask =
    case simulatedTask of
        Succeed a ->
            Task.succeed a

        Fail x ->
            Task.fail x

        HttpTask httpRequest ->
            Http.task
                { method = httpRequest.method
                , headers = List.map (\( key, value ) -> Http.header key value) httpRequest.headers
                , url = httpRequest.url
                , body =
                    case httpRequest.body of
                        EmptyBody ->
                            Http.emptyBody

                        StringBody { contentType, content } ->
                            Http.stringBody contentType content

                        JsonBody value ->
                            Http.jsonBody value
                , resolver = Http.stringResolver Ok
                , timeout = Maybe.map Duration.inMilliseconds httpRequest.timeout
                }
                |> Task.andThen (\response -> httpRequest.onRequestComplete response |> toTask)

        SleepTask duration function ->
            Process.sleep (Duration.inMilliseconds duration)
                |> Task.andThen (\() -> toTask (function ()))

        GetTime gotTime ->
            Time.now |> Task.andThen (\time -> toTask (gotTime time))

        GetTimeZone gotTimeZone ->
            Time.here |> Task.andThen (\time -> toTask (gotTimeZone time))

        GetTimeZoneName gotTimeZoneName ->
            Time.getZoneName |> Task.andThen (\time -> toTask (gotTimeZoneName time))

        SetViewport x y function ->
            Browser.Dom.setViewport (Pixels.inPixels x) (Pixels.inPixels y) |> Task.andThen (\() -> toTask (function ()))

        GetViewport function ->
            Browser.Dom.getViewport |> Task.andThen (\viewport -> toTask (function viewport))

        GetElement function string ->
            Browser.Dom.getElement string
                |> Task.map Ok
                |> Task.onError (Err >> Task.succeed)
                |> Task.andThen (\result -> toTask (function result))
