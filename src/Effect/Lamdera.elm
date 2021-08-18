module Effect.Lamdera exposing (frontend, backend, sendToBackend, sendToFrontend, broadcast, onConnect, onDisconnect, ClientId, SessionId, Url, Document, Key, UrlRequest)

{-| backend

@docs frontend, backend, sendToBackend, sendToFrontend, broadcast, onConnect, onDisconnect, clientConnected_, clientDisconnected_, ClientId, SessionId, Url, Document, Key, UrlRequest

-}

import Browser
import Browser.Events
import Browser.Navigation
import Duration
import Effect.Browser.Events
import Effect.Internal
import Lamdera
import Pixels
import TestId
import Time
import Url


type alias Effect restriction toMsg msg =
    Effect.Internal.Effect restriction toMsg msg


type alias FrontendOnly =
    Effect.Internal.FrontendOnly


type alias BackendOnly =
    Effect.Internal.BackendOnly


type alias Subscription restriction msg =
    Effect.Internal.Subscription restriction msg


{-| Create a Lamdera frontend application
-}
frontend :
    { init : Url.Url -> Key -> ( model, Effect FrontendOnly toBackend frontendMsg )
    , view : model -> Browser.Document frontendMsg
    , update : frontendMsg -> model -> ( model, Effect FrontendOnly toBackend frontendMsg )
    , updateFromBackend : toFrontend -> model -> ( model, Effect FrontendOnly toBackend frontendMsg )
    , subscriptions : model -> Subscription FrontendOnly frontendMsg
    , onUrlRequest : Browser.UrlRequest -> frontendMsg
    , onUrlChange : Url -> frontendMsg
    }
    ->
        { init : Url -> Browser.Navigation.Key -> ( model, Cmd frontendMsg )
        , view : model -> Browser.Document frontendMsg
        , update : frontendMsg -> model -> ( model, Cmd frontendMsg )
        , updateFromBackend : toFrontend -> model -> ( model, Cmd frontendMsg )
        , subscriptions : model -> Sub frontendMsg
        , onUrlRequest : Browser.UrlRequest -> frontendMsg
        , onUrlChange : Url.Url -> frontendMsg
        }
frontend userApp =
    { init =
        \url navigationKey ->
            userApp.init url (Effect.Internal.RealNavigationKey navigationKey)
                |> Tuple.mapSecond Effect.Internal.toCmd
    , view = userApp.view
    , update = \msg model -> userApp.update msg model |> Tuple.mapSecond Effect.Internal.toCmd
    , updateFromBackend = \msg model -> userApp.updateFromBackend msg model |> Tuple.mapSecond Effect.Internal.toCmd
    , subscriptions = userApp.subscriptions >> toSub
    , onUrlRequest = userApp.onUrlRequest
    , onUrlChange = userApp.onUrlChange
    }


{-| Create a Lamdera backend application
-}
backend :
    { init : ( backendModel, Effect BackendOnly toFrontend backendMsg )
    , update : backendMsg -> backendModel -> ( backendModel, Effect BackendOnly toFrontend backendMsg )
    , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Effect BackendOnly toFrontend backendMsg )
    , subscriptions : backendModel -> Subscription BackendOnly backendMsg
    }
    ->
        { init : ( backendModel, Cmd backendMsg )
        , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
        , updateFromFrontend : String -> String -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
        , subscriptions : backendModel -> Sub backendMsg
        }
backend userApp =
    { init = userApp.init |> Tuple.mapSecond Effect.Internal.toCmd
    , update = \msg model -> userApp.update msg model |> Tuple.mapSecond Effect.Internal.toCmd
    , updateFromFrontend =
        \sessionId clientId msg model ->
            userApp.updateFromFrontend
                (TestId.sessionIdFromString sessionId)
                (TestId.clientIdFromString clientId)
                msg
                model
                |> Tuple.mapSecond Effect.Internal.toCmd
    , subscriptions = userApp.subscriptions >> toSub
    }


{-| Send a toBackend msg to the Backend
-}
sendToBackend : toBackend -> Effect FrontendOnly toBackend frontendMsg
sendToBackend =
    Effect.Internal.SendToBackend


{-| Send a toFrontend msg to the Frontend
-}
sendToFrontend : ClientId -> toFrontend -> Effect BackendOnly toFrontend backendMsg
sendToFrontend =
    Effect.Internal.SendToFrontend


{-| Send a toFrontend msg to all currently connected clients
-}
broadcast : toFrontend -> Effect BackendOnly toFrontend backendMsg
broadcast =
    Effect.Internal.Broadcast


{-| Subscribe to Frontend client connected events
-}
onConnect : (SessionId -> ClientId -> backendMsg) -> Subscription BackendOnly backendMsg
onConnect =
    Effect.Internal.OnConnect


{-| Subscribe to Frontend client disconnected events
-}
onDisconnect : (SessionId -> ClientId -> backendMsg) -> Subscription BackendOnly backendMsg
onDisconnect =
    Effect.Internal.OnDisconnect


{-| -}
type alias ClientId =
    TestId.ClientId


{-| -}
type alias SessionId =
    TestId.SessionId


{-| Alias of elm/url:Url.Url
-}
type alias Url =
    Url.Url


{-| Alias of elm/browser:Browser.Document
-}
type alias Document msg =
    Browser.Document msg


{-| Alias of elm/browser:Browser.UrlRequest
-}
type alias UrlRequest =
    Browser.UrlRequest


{-| Alias of elm/browser:Browser.Navigation.Key
-}
type alias Key =
    Effect.Internal.NavigationKey


toSub : Subscription restriction msg -> Sub msg
toSub sub =
    case sub of
        Effect.Internal.SubBatch subs ->
            List.map toSub subs |> Sub.batch

        Effect.Internal.SubNone ->
            Sub.none

        Effect.Internal.TimeEvery duration msg ->
            Time.every (Duration.inMilliseconds duration) msg

        Effect.Internal.OnAnimationFrame msg ->
            Browser.Events.onAnimationFrame msg

        Effect.Internal.OnAnimationFrameDelta msg ->
            Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> msg)

        Effect.Internal.OnKeyPress decoder ->
            Browser.Events.onKeyPress decoder

        Effect.Internal.OnKeyDown decoder ->
            Browser.Events.onKeyDown decoder

        Effect.Internal.OnKeyUp decoder ->
            Browser.Events.onKeyUp decoder

        Effect.Internal.OnClick decoder ->
            Browser.Events.onClick decoder

        Effect.Internal.OnMouseMove decoder ->
            Browser.Events.onMouseMove decoder

        Effect.Internal.OnMouseDown decoder ->
            Browser.Events.onMouseDown decoder

        Effect.Internal.OnMouseUp decoder ->
            Browser.Events.onMouseUp decoder

        Effect.Internal.OnVisibilityChange msg ->
            Browser.Events.onVisibilityChange
                (\visibility ->
                    case visibility of
                        Browser.Events.Visible ->
                            msg Effect.Internal.Visible

                        Browser.Events.Hidden ->
                            msg Effect.Internal.Hidden
                )

        Effect.Internal.OnResize msg ->
            Browser.Events.onResize (\w h -> msg (Pixels.pixels w) (Pixels.pixels h))

        Effect.Internal.SubPort _ portFunction _ ->
            portFunction

        Effect.Internal.OnConnect msg ->
            Lamdera.onConnect
                (\sessionId clientId -> msg (TestId.sessionIdFromString sessionId) (TestId.clientIdFromString clientId))

        Effect.Internal.OnDisconnect msg ->
            Lamdera.onDisconnect
                (\sessionId clientId -> msg (TestId.sessionIdFromString sessionId) (TestId.clientIdFromString clientId))
