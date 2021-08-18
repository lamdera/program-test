module Effect.Internal exposing
    ( BackendOnly
    , Effect(..)
    , File(..)
    , FrontendOnly
    , HttpBody(..)
    , HttpRequest
    , NavigationKey(..)
    , Subscription(..)
    , Task(..)
    , toCmd
    , toSub
    )

import Browser.Dom
import Browser.Events
import Browser.Navigation
import Bytes exposing (Bytes)
import Bytes.Encode
import Duration exposing (Duration)
import File
import File.Download
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


type FrontendOnly
    = FrontendOnly Never


type BackendOnly
    = BackendOnly Never


type Subscription restriction msg
    = SubBatch (List (Subscription restriction msg))
    | SubNone
    | TimeEvery Duration (Time.Posix -> msg)
    | OnResize (Quantity Int Pixels -> Quantity Int Pixels -> msg)
    | SubPort String (Sub msg) (Json.Decode.Value -> msg)
    | OnConnect (SessionId -> ClientId -> msg)
    | OnDisconnect (SessionId -> ClientId -> msg)


type Effect restriction toMsg msg
    = Batch (List (Effect restriction toMsg msg))
    | None
    | SendToBackend toMsg
    | NavigationPushUrl NavigationKey String
    | NavigationReplaceUrl NavigationKey String
    | NavigationBack NavigationKey Int
    | NavigationForward NavigationKey Int
    | NavigationLoad String
    | NavigationReload
    | NavigationReloadAndSkipCache
    | Task (Task restriction msg msg)
    | Port String (Json.Encode.Value -> Cmd msg) Json.Encode.Value
    | SendToFrontend ClientId toMsg
    | Broadcast toMsg
    | FileDownloadUrl { href : String }
    | FileDownloadString { name : String, mimeType : String, content : String }
    | FileDownloadBytes { name : String, mimeType : String, content : Bytes }
    | FileSelectFile (List String) (File -> msg)
    | FileSelectFiles (List String) (File -> List File -> msg)


type Task restriction x a
    = Succeed a
    | Fail x
    | HttpTask (HttpRequest restriction x a)
    | SleepTask Duration (() -> Task restriction x a)
    | GetTime (Time.Posix -> Task restriction x a)
    | GetTimeZone (Time.Zone -> Task restriction x a)
    | GetTimeZoneName (Time.ZoneName -> Task restriction x a)
    | GetViewport (Browser.Dom.Viewport -> Task restriction x a)
    | SetViewport (Quantity Float Pixels) (Quantity Float Pixels) (() -> Task restriction x a)
    | GetElement (Result Browser.Dom.Error Browser.Dom.Element -> Task restriction x a) String
    | FileToString File (String -> Task restriction x a)
    | FileToBytes File (Bytes -> Task restriction x a)
    | FileToUrl File (String -> Task restriction x a)


type NavigationKey
    = RealNavigationKey Browser.Navigation.Key
    | MockNavigationKey


type File
    = RealFile File.File
    | MockFile { name : String, mimeType : String, content : String, lastModified : Time.Posix }


type alias HttpRequest restriction x a =
    { method : String
    , url : String
    , body : HttpBody
    , headers : List ( String, String )
    , onRequestComplete : Http.Response String -> Task restriction x a
    , timeout : Maybe Duration
    }


type HttpBody
    = EmptyBody
    | StringBody
        { contentType : String
        , content : String
        }
    | JsonBody Json.Encode.Value


toCmd : Effect restriction toMsg msg -> Cmd msg
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

        NavigationBack navigationKey int ->
            case navigationKey of
                RealNavigationKey key ->
                    Browser.Navigation.back key int

                MockNavigationKey ->
                    Cmd.none

        NavigationForward navigationKey int ->
            case navigationKey of
                RealNavigationKey key ->
                    Browser.Navigation.forward key int

                MockNavigationKey ->
                    Cmd.none

        NavigationReload ->
            Browser.Navigation.reload

        NavigationReloadAndSkipCache ->
            Browser.Navigation.reloadAndSkipCache

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

        FileDownloadUrl { href } ->
            File.Download.url href

        FileDownloadString { name, mimeType, content } ->
            File.Download.string name mimeType content

        FileDownloadBytes { name, mimeType, content } ->
            File.Download.bytes name mimeType content

        FileSelectFile mimeTypes msg ->
            File.Select.file mimeTypes (RealFile >> msg)

        FileSelectFiles mimeTypes msg ->
            File.Select.files mimeTypes (\file restOfFiles -> msg (RealFile file) (List.map RealFile restOfFiles))

        Broadcast toMsg ->
            Lamdera.broadcast toMsg


toTask : Task restriction x b -> Task.Task x b
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

        FileToString file function ->
            case file of
                RealFile file_ ->
                    File.toString file_ |> Task.andThen (\result -> toTask (function result))

                MockFile { content } ->
                    Task.succeed content |> Task.andThen (\result -> toTask (function result))

        FileToBytes file function ->
            case file of
                RealFile file_ ->
                    File.toBytes file_ |> Task.andThen (\result -> toTask (function result))

                MockFile { content } ->
                    Bytes.Encode.string content
                        |> Bytes.Encode.encode
                        |> Task.succeed
                        |> Task.andThen (\result -> toTask (function result))

        FileToUrl file function ->
            case file of
                RealFile file_ ->
                    File.toUrl file_ |> Task.andThen (\result -> toTask (function result))

                MockFile { content } ->
                    -- This isn't the correct behavior but it should be okay as MockFile should never be used here.
                    Task.succeed content |> Task.andThen (\result -> toTask (function result))


toSub : Subscription restriction msg -> Sub msg
toSub sub =
    case sub of
        SubBatch subs ->
            List.map toSub subs |> Sub.batch

        SubNone ->
            Sub.none

        TimeEvery duration msg ->
            Time.every (Duration.inMilliseconds duration) msg

        OnResize msg ->
            Browser.Events.onResize (\w h -> msg (Pixels.pixels w) (Pixels.pixels h))

        SubPort _ portFunction _ ->
            portFunction

        OnConnect msg ->
            Lamdera.onConnect
                (\sessionId clientId -> msg (TestId.sessionIdFromString sessionId) (TestId.clientIdFromString clientId))

        OnDisconnect msg ->
            Lamdera.onDisconnect
                (\sessionId clientId -> msg (TestId.sessionIdFromString sessionId) (TestId.clientIdFromString clientId))
