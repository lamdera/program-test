module Effect.LocalDev exposing (DevBar, InitConfig, Model, Msg, SubscriptionsConfig, UpdateConfig, ViewConfig, fileReadWriteErrorView, hideFreezeAndResetButtons, init, initDevBar, isFullyInitialized, maybeTestEditorView, onConnection, onDisconnection, receivedToFrontend, recordingButton, recordingPill, resetDebugStoreBE, subscriptions, update)

{-| Ignore this module, for internal use only.

@docs DevBar, InitConfig, Model, Msg, SubscriptionsConfig, UpdateConfig, ViewConfig, fileReadWriteErrorView, hideFreezeAndResetButtons, init, initDevBar, isFullyInitialized, maybeTestEditorView, onConnection, onDisconnection, receivedToFrontend, recordingButton, recordingPill, resetDebugStoreBE, subscriptions, update

-}

import Array exposing (Array)
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Elm.CodeGen as Codegen exposing (Expression)
import Elm.Parser
import Elm.Pretty
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Lamdera exposing (ClientId, SessionId, Url)
import Lamdera.Debug as LD
import Lamdera.Json as Json exposing (Decoder)
import Lamdera.Wire3 exposing (Bytes)
import Pretty
import Process
import Set exposing (Set)
import Svg as S
import Svg.Attributes as A
import Task
import Time
import Url


{-| -}
type alias ConnectionMsg =
    { s : SessionId, c : ClientId }


{-| -}
type alias WireMsg =
    { t : String, s : String, c : String, b : Bytes }


{-| -}
type Msg
    = Noop
    | OnConnectionDelayed ConnectionMsg
      --
    | PressedStartRecording
    | NotifiedFollowersOfRecordingStart
    | PressedStopRecording
    | GotEvent Json.Decode.Value
    | TestEditorMsg TestEditorMsg
    | GotTimeAndViewport ( Time.Posix, Browser.Dom.Viewport )
    | TimedOutWaitingOnRecordingStatus
    | CheckFileReadWriteEnabled (Result Http.Error String)
    | PressedCopy String
    | PressedCloseModal


{-| -}
type Model msg
    = WaitingOnRecordingStatus (Cmd msg)
    | NormalModel NormalModelData


{-| -}
type alias NormalModelData =
    { recordedEvents : LoadedData
    , showFileReadWriteError : Bool
    , lastCopied : Maybe String
    , waitingConnections : Set ClientId
    }


{-| -}
type alias DevBar =
    { isRecordingEvents : Maybe RecordingState
    }


type alias RecordingState =
    { history : Array Event
    , recordingStopped : Bool
    }



-- INIT


{-| -}
type alias InitConfig msg =
    { clientId : ClientId
    , devBar : DevBar
    , initCmds : Cmd msg
    , isLeader : Bool
    , mapMsg : Msg -> msg
    , sendToFrontend : WireMsg -> Cmd msg
    , sessionId : SessionId
    , startRecording : () -> Cmd msg
    }


{-| -}
initDevBar : DevBar
initDevBar =
    { isRecordingEvents = Nothing }


{-| -}
init : InitConfig msg -> ( Model msg, Cmd msg )
init config =
    if config.isLeader then
        normalInit config |> Tuple.mapFirst NormalModel

    else
        ( WaitingOnRecordingStatus config.initCmds
        , Cmd.batch
            [ config.sendToFrontend
                { t = "ToFrontend"
                , s = "LocalDev"
                , c = "b"
                , b =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 3
                            , encodeString config.sessionId
                            , encodeString config.clientId
                            ]
                        )
                }
            , Process.sleep 2000
                |> Task.perform (\() -> config.mapMsg TimedOutWaitingOnRecordingStatus)
            ]
        )


normalInit : InitConfig msg -> ( NormalModelData, Cmd msg )
normalInit config =
    let
        recorded : LoadedData
        recorded =
            { copyCounter = 0
            , settings =
                { includeClientPos = False
                , includePagePos = False
                , includeScreenPos = False
                , showAllCode = False
                }
            , parsedCode = WaitingOnFile
            , mouseDownOnEvent = False
            , commitStatus = NotCommitted
            }
    in
    ( { recordedEvents = recorded
      , showFileReadWriteError = False
      , lastCopied = Nothing
      , waitingConnections = Set.empty
      }
    , Cmd.batch
        [ config.initCmds
        , if config.isLeader then
            case config.devBar.isRecordingEvents of
                Just recording ->
                    if recording.recordingStopped then
                        loadTestsFile GotTestFile |> Cmd.map (config.mapMsg << TestEditorMsg)

                    else
                        recordingInitCmds config

                Nothing ->
                    Cmd.none

          else
            Cmd.none
        ]
    )


recordingInitCmds : InitConfig msg -> Cmd msg
recordingInitCmds config =
    Cmd.batch
        [ config.startRecording ()
        , Task.map2 Tuple.pair Time.now Browser.Dom.getViewport
            |> Task.perform (config.mapMsg << GotTimeAndViewport)
        ]


{-| -}
isFullyInitialized : Model msg -> Bool
isFullyInitialized model =
    case model of
        WaitingOnRecordingStatus _ ->
            False

        NormalModel _ ->
            True



-- UPDATE


{-| -}
type alias UpdateConfig msg model =
    { clientId : ClientId
    , copyToClipboard : String -> Cmd msg
    , debugSaveDevBar : model -> model
    , devBar : DevBar
    , isLeader : Bool
    , mapMsg : Msg -> msg
    , onConnection : ConnectionMsg -> Cmd msg
    , originalUrl : Url
    , resetBackend : model -> ( model, Cmd msg )
    , sendToFrontend : WireMsg -> Cmd msg
    , sessionId : SessionId
    , setDevBar : DevBar -> model -> model
    , setExpanded : Bool -> model -> model
    , setFreeze : Bool -> model -> model
    , setModel : Model msg -> model -> model
    , startRecording : () -> Cmd msg
    , stopRecording : () -> Cmd msg
    }


{-| -}
onConnection : UpdateConfig msg model -> ConnectionMsg -> Model msg -> (model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
onConnection config msg model localDevHandler localDevModel =
    case model of
        WaitingOnRecordingStatus _ ->
            ( localDevModel, Cmd.none )

        NormalModel normalModel ->
            if config.isLeader then
                if msg.c == config.clientId then
                    localDevHandler localDevModel

                else
                    ( localDevModel
                        |> config.setModel
                            (NormalModel
                                { normalModel
                                    | waitingConnections =
                                        Set.insert msg.c normalModel.waitingConnections
                                }
                            )
                    , delayMsg 2000 (config.mapMsg (OnConnectionDelayed msg))
                    )

            else
                ( localDevModel, Cmd.none )


{-| -}
onDisconnection : UpdateConfig msg model -> ConnectionMsg -> Model msg -> (model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
onDisconnection config msg model localDevHandler localDevModel =
    case model of
        WaitingOnRecordingStatus _ ->
            ( localDevModel, Cmd.none )

        NormalModel normalModel ->
            localDevModel
                |> config.setModel
                    (NormalModel
                        { normalModel
                            | waitingConnections =
                                Set.remove msg.c normalModel.waitingConnections
                        }
                    )
                |> localDevHandler


{-| -}
receivedToFrontend : UpdateConfig msg model -> WireMsg -> Model msg -> (model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
receivedToFrontend config msg model localDevHandler localDevModel =
    case model of
        WaitingOnRecordingStatus initCmds ->
            if msg.s == "LocalDev" then
                waitingOnRecordingStatusUpdate config initCmds msg.b localDevModel

            else
                ( localDevModel, Cmd.none )

        NormalModel _ ->
            if msg.s == "LocalDev" then
                receivedMsgFromLocalDev config msg.b localDevModel

            else
                localDevHandler localDevModel


waitingOnRecordingStatusUpdate : UpdateConfig msg model -> Cmd msg -> Bytes -> model -> ( model, Cmd msg )
waitingOnRecordingStatusUpdate config initCmds payload localDevModel =
    Bytes.Decode.decode
        (Bytes.Decode.unsignedInt8
            |> Bytes.Decode.andThen
                (\flag ->
                    case flag of
                        4 ->
                            -- Response if there is a recording in progress
                            Bytes.Decode.map
                                (\text ->
                                    case Json.Decode.decodeString (Json.Decode.array fullEventDecoder) text of
                                        Ok history ->
                                            let
                                                ( newLocalDevModel, cmd ) =
                                                    resumeNormalInitFromUpdate config initCmds localDevModel
                                            in
                                            ( newLocalDevModel
                                                |> config.setDevBar
                                                    { isRecordingEvents =
                                                        Just { history = history, recordingStopped = False }
                                                    }
                                                |> config.debugSaveDevBar
                                            , Cmd.batch
                                                [ cmd
                                                , recordingInitCmds (initConfigFromUpdateConfig config initCmds)
                                                ]
                                            )

                                        Err _ ->
                                            resumeNormalInitFromUpdate config initCmds localDevModel
                                )
                                decodeString

                        5 ->
                            -- Response if there is no recording in progress
                            resumeNormalInitFromUpdate config initCmds localDevModel
                                |> Bytes.Decode.succeed

                        _ ->
                            Bytes.Decode.succeed
                                ( localDevModel
                                    |> config.setModel (WaitingOnRecordingStatus initCmds)
                                , Cmd.none
                                )
                )
        )
        payload
        |> Maybe.withDefault
            ( localDevModel
                |> config.setModel (WaitingOnRecordingStatus initCmds)
            , Cmd.none
            )


receivedMsgFromLocalDev : UpdateConfig msg model -> Bytes -> model -> ( model, Cmd msg )
receivedMsgFromLocalDev config payload localDevModel =
    Bytes.Decode.decode
        (Bytes.Decode.unsignedInt8
            |> Bytes.Decode.andThen
                (\flag ->
                    case flag of
                        0 ->
                            -- Start recording
                            localDevModel
                                |> config.setDevBar { isRecordingEvents = Just initRecording }
                                |> config.debugSaveDevBar
                                |> config.resetBackend
                                |> Bytes.Decode.succeed

                        1 ->
                            -- Stop recording
                            decodeString
                                |> Bytes.Decode.map
                                    (\clientId ->
                                        if clientId == config.clientId then
                                            ( localDevModel, Cmd.none )

                                        else
                                            ( localDevModel
                                                |> config.setDevBar { isRecordingEvents = Nothing }
                                                |> config.debugSaveDevBar
                                            , config.stopRecording ()
                                            )
                                    )

                        2 ->
                            -- Got a user event
                            Bytes.Decode.map2
                                (\clientId json ->
                                    case
                                        ( config.devBar.isRecordingEvents
                                        , Json.Decode.decodeString (eventDecoder clientId) json
                                        )
                                    of
                                        ( Just recording, Ok event ) ->
                                            ( localDevModel
                                                |> config.setDevBar
                                                    { isRecordingEvents =
                                                        addEvent event recording |> Just
                                                    }
                                                |> config.debugSaveDevBar
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( localDevModel, Cmd.none )
                                )
                                decodeString
                                decodeString

                        3 ->
                            -- Request if there is a recording in progress
                            Bytes.Decode.map2
                                (\sessionId clientId ->
                                    ( localDevModel
                                    , if config.isLeader then
                                        Cmd.batch
                                            [ case config.devBar.isRecordingEvents of
                                                Just recording ->
                                                    if recording.recordingStopped then
                                                        config.sendToFrontend
                                                            { t = "ToFrontend"
                                                            , s = "LocalDev"
                                                            , c = clientId
                                                            , b =
                                                                Bytes.Encode.encode
                                                                    (Bytes.Encode.unsignedInt8 5)
                                                            }

                                                    else
                                                        config.sendToFrontend
                                                            { t = "ToFrontend"
                                                            , s = "LocalDev"
                                                            , c = clientId
                                                            , b =
                                                                Bytes.Encode.encode
                                                                    (Bytes.Encode.sequence
                                                                        [ Bytes.Encode.unsignedInt8 4
                                                                        , Json.Encode.array
                                                                            eventEncoder
                                                                            recording.history
                                                                            |> Json.Encode.encode 0
                                                                            |> encodeString
                                                                        ]
                                                                    )
                                                            }

                                                Nothing ->
                                                    config.sendToFrontend
                                                        { t = "ToFrontend"
                                                        , s = "LocalDev"
                                                        , c = clientId
                                                        , b =
                                                            Bytes.Encode.encode
                                                                (Bytes.Encode.unsignedInt8 5)
                                                        }
                                            , sendMsg (config.mapMsg (OnConnectionDelayed { s = sessionId, c = clientId }))
                                            ]

                                      else
                                        Cmd.none
                                    )
                                )
                                decodeString
                                decodeString

                        _ ->
                            Bytes.Decode.succeed ( localDevModel, Cmd.none )
                )
        )
        payload
        |> Maybe.withDefault ( localDevModel, Cmd.none )


decodeString : Bytes.Decode.Decoder String
decodeString =
    Bytes.Decode.andThen
        Bytes.Decode.string
        (Bytes.Decode.unsignedInt32 Bytes.BE)


encodeString : String -> Bytes.Encode.Encoder
encodeString text =
    Bytes.Encode.sequence
        [ Bytes.Encode.unsignedInt32 Bytes.BE (Bytes.Encode.getStringWidth text)
        , Bytes.Encode.string text
        ]


broadcastEvent : UpdateConfig msg model -> String -> Cmd msg
broadcastEvent config eventText =
    config.sendToFrontend
        { t = "ToFrontend"
        , s = "LocalDev"
        , c = "b"
        , b =
            Bytes.Encode.encode
                (Bytes.Encode.sequence
                    [ Bytes.Encode.unsignedInt8 2
                    , encodeString config.clientId
                    , encodeString eventText
                    ]
                )
        }


{-| -}
resetDebugStoreBE : UpdateConfig msg model -> model -> model
resetDebugStoreBE config localDevModel =
    localDevModel
        |> config.setDevBar initDevBar
        |> config.debugSaveDevBar


{-| -}
update : UpdateConfig msg model -> Msg -> Model msg -> model -> ( model, Cmd msg )
update config msg model localDevModel =
    case model of
        WaitingOnRecordingStatus initCmds ->
            case msg of
                TimedOutWaitingOnRecordingStatus ->
                    resumeNormalInitFromUpdate config initCmds localDevModel

                _ ->
                    ( localDevModel, Cmd.none )

        NormalModel normalModel ->
            normalUpdate config msg normalModel localDevModel


resumeNormalInitFromUpdate : UpdateConfig msg model -> Cmd msg -> model -> ( model, Cmd msg )
resumeNormalInitFromUpdate config initCmds localDevModel =
    normalInit (initConfigFromUpdateConfig config initCmds)
        |> Tuple.mapFirst
            (\normalModel ->
                config.setModel (NormalModel normalModel) localDevModel
            )


initConfigFromUpdateConfig : UpdateConfig msg model -> Cmd msg -> InitConfig msg
initConfigFromUpdateConfig config initCmds =
    { clientId = config.clientId
    , devBar = config.devBar
    , initCmds = initCmds
    , isLeader = config.isLeader
    , mapMsg = config.mapMsg
    , sendToFrontend = config.sendToFrontend
    , sessionId = config.sessionId
    , startRecording = config.startRecording
    }


normalUpdate : UpdateConfig msg model -> Msg -> NormalModelData -> model -> ( model, Cmd msg )
normalUpdate config msg m localDevModel =
    case msg of
        Noop ->
            ( localDevModel, Cmd.none )

        OnConnectionDelayed d ->
            if Set.member d.c m.waitingConnections then
                ( localDevModel
                    |> config.setModel
                        (NormalModel
                            { m
                                | waitingConnections =
                                    Set.remove d.c m.waitingConnections
                            }
                        )
                , config.onConnection d
                )

            else
                ( localDevModel, Cmd.none )

        PressedStartRecording ->
            ( localDevModel, loadTestsFile CheckFileReadWriteEnabled |> Cmd.map config.mapMsg )

        CheckFileReadWriteEnabled result ->
            let
                fileReadWriteWorks =
                    case result of
                        Ok _ ->
                            True

                        Err (Http.BadStatus 404) ->
                            True

                        Err _ ->
                            False
            in
            if fileReadWriteWorks then
                ( localDevModel
                    |> config.setDevBar { isRecordingEvents = Just initRecording }
                    |> config.setFreeze False
                    |> config.debugSaveDevBar
                , Cmd.batch
                    [ config.sendToFrontend
                        { t = "ToFrontend"
                        , s = "LocalDev"
                        , c = "b"
                        , b = Bytes.Encode.encode (Bytes.Encode.unsignedInt8 0)
                        }
                    , delayMsg 1000 (config.mapMsg NotifiedFollowersOfRecordingStart)
                    ]
                )

            else
                ( localDevModel
                    |> config.setModel (NormalModel { m | showFileReadWriteError = True })
                , Cmd.none
                )

        NotifiedFollowersOfRecordingStart ->
            config.resetBackend localDevModel

        PressedStopRecording ->
            case config.devBar.isRecordingEvents of
                Just recording ->
                    ( localDevModel
                        |> config.setDevBar { isRecordingEvents = Just { recording | recordingStopped = True } }
                        |> config.setExpanded False
                        |> config.debugSaveDevBar
                    , Cmd.batch
                        [ loadTestsFile (\result -> GotTestFile result |> TestEditorMsg |> config.mapMsg)
                        , config.stopRecording ()
                        , config.sendToFrontend
                            { t = "ToFrontend"
                            , s = "LocalDev"
                            , c = "b"
                            , b =
                                Bytes.Encode.encode
                                    (Bytes.Encode.sequence
                                        [ Bytes.Encode.unsignedInt8 1
                                        , encodeString config.clientId
                                        ]
                                    )
                            }
                        ]
                    )

                Nothing ->
                    ( localDevModel, Cmd.none )

        GotEvent json ->
            case config.devBar.isRecordingEvents of
                Just recording ->
                    if recording.recordingStopped then
                        ( localDevModel, Cmd.none )

                    else
                        ( localDevModel, Json.Encode.encode 0 json |> broadcastEvent config )

                _ ->
                    ( localDevModel, Cmd.none )

        TestEditorMsg testEditorMsg ->
            case config.devBar.isRecordingEvents of
                Just recording ->
                    let
                        ( testEditor2, maybeDevbar, cmd ) =
                            updateLoaded testEditorMsg m.recordedEvents recording
                    in
                    ( localDevModel
                        |> config.setModel (NormalModel { m | recordedEvents = testEditor2 })
                        |> config.setDevBar
                            (case maybeDevbar of
                                Change recording2 ->
                                    { isRecordingEvents = recording2 }

                                NoChange ->
                                    config.devBar
                            )
                        |> config.debugSaveDevBar
                    , Cmd.map (config.mapMsg << TestEditorMsg) cmd
                    )

                Nothing ->
                    ( localDevModel, Cmd.none )

        GotTimeAndViewport ( time, { viewport } ) ->
            case config.devBar.isRecordingEvents of
                Just recording ->
                    if recording.recordingStopped then
                        ( localDevModel, Cmd.none )

                    else
                        ( localDevModel
                        , { isHidden = False
                          , clientId = config.clientId
                          , timestamp = time
                          , eventType =
                                Connect
                                    { url = Url.toString config.originalUrl
                                    , sessionId = config.sessionId
                                    , windowWidth = round viewport.width
                                    , windowHeight = round viewport.height
                                    }
                          }
                            |> eventEncoder
                            |> Json.Encode.encode 0
                            |> broadcastEvent config
                        )

                Nothing ->
                    ( localDevModel, Cmd.none )

        TimedOutWaitingOnRecordingStatus ->
            ( localDevModel, Cmd.none )

        PressedCopy text ->
            ( localDevModel
                |> config.setModel (NormalModel { m | lastCopied = Just text })
            , config.copyToClipboard text
            )

        PressedCloseModal ->
            ( localDevModel
                |> config.setModel (NormalModel { m | showFileReadWriteError = False })
            , Cmd.none
            )


initRecording : RecordingState
initRecording =
    { history = Array.empty
    , recordingStopped = False
    }


sendMsg : b -> Cmd b
sendMsg msg =
    Task.succeed msg |> Task.perform identity


delayMsg : Float -> b -> Cmd b
delayMsg time msg =
    Process.sleep time |> Task.perform (always msg)



-- SUBSCRIPTIONS


{-| -}
type alias SubscriptionsConfig msg =
    { devBar : DevBar
    , gotEvent : (Json.Value -> msg) -> Sub msg
    , mapMsg : Msg -> msg
    , receiveToFrontend : Sub msg
    }


{-| -}
subscriptions : SubscriptionsConfig msg -> Model msg -> Sub msg
subscriptions config model =
    case model of
        WaitingOnRecordingStatus _ ->
            config.receiveToFrontend

        NormalModel _ ->
            normalSubscriptions config


normalSubscriptions : SubscriptionsConfig msg -> Sub msg
normalSubscriptions config =
    case config.devBar.isRecordingEvents of
        Just recording ->
            Sub.batch
                [ config.gotEvent (config.mapMsg << GotEvent)
                , if recording.recordingStopped then
                    Browser.Events.onMouseUp (Json.Decode.succeed (config.mapMsg (TestEditorMsg MouseUpEvent)))

                  else
                    Sub.none
                ]

        Nothing ->
            Sub.none



-- VIEW


{-| -}
type alias ViewConfig =
    { charcoal : String
    , grey : String
    , white : String
    }


eye : Html msg
eye =
    S.svg
        [ A.viewBox "0 0 256 256"
        , A.width "16"
        , A.height "16"
        , A.fill "currentColor"
        , A.stroke "currentColor"
        ]
        [ S.path [ A.d "M247.31,124.76c-.35-.79-8.82-19.58-27.65-38.41C194.57,61.26,162.88,48,128,48S61.43,61.26,36.34,86.35C17.51,105.18,9,124,8.69,124.76a8,8,0,0,0,0,6.5c.35.79,8.82,19.57,27.65,38.4C61.43,194.74,93.12,208,128,208s66.57-13.26,91.66-38.34c18.83-18.83,27.3-37.61,27.65-38.4A8,8,0,0,0,247.31,124.76ZM128,192c-30.78,0-57.67-11.19-79.93-33.25A133.47,133.47,0,0,1,25,128,133.33,133.33,0,0,1,48.07,97.25C70.33,75.19,97.22,64,128,64s57.67,11.19,79.93,33.25A133.46,133.46,0,0,1,231.05,128C223.84,141.46,192.43,192,128,192Zm0-112a48,48,0,1,0,48,48A48.05,48.05,0,0,0,128,80Zm0,80a32,32,0,1,1,32-32A32,32,0,0,1,128,160Z" ] [] ]


eyeClosed : Html msg
eyeClosed =
    S.svg
        [ A.viewBox "0 0 256 256"
        , A.width "16"
        , A.height "16"
        , A.fill "currentColor"
        , A.stroke "currentColor"
        ]
        [ S.path [ A.d "M228,175a8,8,0,0,1-10.92-3l-19-33.2A123.23,123.23,0,0,1,162,155.46l5.87,35.22a8,8,0,0,1-6.58,9.21A8.4,8.4,0,0,1,160,200a8,8,0,0,1-7.88-6.69l-5.77-34.58a133.06,133.06,0,0,1-36.68,0l-5.77,34.58A8,8,0,0,1,96,200a8.4,8.4,0,0,1-1.32-.11,8,8,0,0,1-6.58-9.21L94,155.46a123.23,123.23,0,0,1-36.06-16.69L39,172A8,8,0,1,1,25.06,164l20-35a153.47,153.47,0,0,1-19.3-20A8,8,0,1,1,38.22,99c16.6,20.54,45.64,45,89.78,45s73.18-24.49,89.78-45A8,8,0,1,1,230.22,109a153.47,153.47,0,0,1-19.3,20l20,35A8,8,0,0,1,228,175Z" ] [] ]


{-| -}
recordingPill : DevBar -> Maybe Msg
recordingPill devBar =
    if devBar.isRecordingEvents /= Nothing then
        Just PressedStopRecording

    else
        Nothing


{-| -}
recordingButton : DevBar -> (Msg -> Html msg) -> (Msg -> Html msg) -> Html msg
recordingButton devBar stopFun startFun =
    case devBar.isRecordingEvents of
        Just _ ->
            stopFun PressedStopRecording

        Nothing ->
            startFun PressedStartRecording


{-| -}
hideFreezeAndResetButtons : DevBar -> Bool
hideFreezeAndResetButtons devBar =
    devBar.isRecordingEvents /= Nothing


{-| -}
maybeTestEditorView : ViewConfig -> DevBar -> Model msg -> Maybe (List (Html Msg))
maybeTestEditorView config devBar model =
    case model of
        WaitingOnRecordingStatus _ ->
            Just []

        NormalModel normalModel ->
            devBar.isRecordingEvents
                |> Maybe.andThen
                    (\recording ->
                        if recording.recordingStopped then
                            Just
                                [ testEditorView config recording normalModel.recordedEvents
                                    |> Html.map TestEditorMsg
                                ]

                        else
                            Nothing
                    )


{-| -}
fileReadWriteErrorView : ViewConfig -> Model msg -> List (Html Msg)
fileReadWriteErrorView config model =
    case model of
        WaitingOnRecordingStatus _ ->
            []

        NormalModel normalModel ->
            if normalModel.showFileReadWriteError then
                [ Html.div
                    [ Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.style "width" "100vw"
                    , Html.Attributes.style "height" "100vh"
                    , Html.Attributes.style "background-color" "#000000aa"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "z-index" "1001"
                    , Html.Events.onClick PressedCloseModal
                    ]
                    [ Html.div
                        [ Html.Attributes.style "padding" "20px 30px"
                        , Html.Attributes.style "border-radius" "5px"
                        , Html.Attributes.style "color" config.white
                        , Html.Attributes.style "background-color" config.charcoal
                        , Html.Attributes.style "font-family" "sans-serif"
                        , Html.Events.stopPropagationOn
                            "click"
                            (Json.Decode.succeed ( Noop, True ))
                        ]
                        [ Html.text "In order to record a test, please stop lamdera live and instead run one of the following:"
                        , Html.br [] []
                        , Html.br [] []
                        , copyableCode config normalModel "Terminal" "EXPERIMENTAL=1 lamdera live"
                        , copyableCode config normalModel "PowerShell" "$Env:EXPERIMENTAL=1\nlamdera live"
                        , copyableCode config normalModel "Command Prompt" "set EXPERIMENTAL=1\nlamdera live"
                        ]
                    ]
                ]

            else
                []


copyableCode : ViewConfig -> NormalModelData -> String -> String -> Html Msg
copyableCode config model title text =
    Html.div
        []
        [ Html.div [ Html.Attributes.style "font-size" "14px" ] [ Html.text title ]
        , Html.pre
            [ Html.Attributes.style "background-color" lightCharcoal
            , Html.Attributes.style "padding" "4px"
            , Html.Attributes.style "margin" "2px 0 12px 0"
            , Html.Attributes.style "position" "relative"
            ]
            [ Html.text text
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "0px"
                , Html.Attributes.style "right" "0px"
                , Html.Attributes.style "cursor" "pointer"
                , Html.Attributes.style "height" "23px"
                , Html.Attributes.style "padding" "0 6px"
                , Html.Events.onClick (PressedCopy text)
                , Html.Attributes.style "background-color" config.grey
                ]
                [ if Just text == model.lastCopied then
                    Html.div
                        [ Html.Attributes.style "padding" "3px 0 0 0"
                        , Html.Attributes.style "font-size" "14px"
                        ]
                        [ Html.text "Copied!" ]

                  else
                    Html.div
                        [ Html.Attributes.style "width" "18px"
                        , Html.Attributes.style "padding" "2px 0 0 0"
                        ]
                        [ copyIcon ]
                ]
            ]
        ]


copyIcon : Html msg
copyIcon =
    S.svg [ A.fill "none", A.viewBox "0 0 24 24", A.strokeWidth "1.5", A.stroke "currentColor" ] [ S.path [ A.strokeLinecap "round", A.strokeLinejoin "round", A.d "M15.75 17.25v3.375c0 .621-.504 1.125-1.125 1.125h-9.75a1.125 1.125 0 0 1-1.125-1.125V7.875c0-.621.504-1.125 1.125-1.125H6.75a9.06 9.06 0 0 1 1.5.124m7.5 10.376h3.375c.621 0 1.125-.504 1.125-1.125V11.25c0-4.46-3.243-8.161-7.5-8.876a9.06 9.06 0 0 0-1.5-.124H9.375c-.621 0-1.125.504-1.125 1.125v3.5m7.5 10.375H9.375a1.125 1.125 0 0 1-1.125-1.125v-9.25m12 6.625v-1.875a3.375 3.375 0 0 0-3.375-3.375h-1.5a1.125 1.125 0 0 1-1.125-1.125v-1.5a3.375 3.375 0 0 0-3.375-3.375H9.75" ] [] ]


lightCharcoal : String
lightCharcoal =
    "#434a4d"



--- Program test generator ---


eventDecoder : ClientId -> Decoder Event
eventDecoder clientId =
    Json.succeed (Event False clientId)
        |> andMap "timestamp" timestampDecoder
        |> andMap "eventType" eventTypeDecoder


fullEventDecoder : Decoder Event
fullEventDecoder =
    Json.succeed Event
        |> andMap "isHidden" Json.Decode.bool
        |> andMap "clientId" Json.Decode.string
        |> andMap "timestamp" timestampDecoder
        |> andMap "eventType" eventTypeDecoder


timestampDecoder : Decoder Time.Posix
timestampDecoder =
    Json.Decode.map (\timestamp -> Time.millisToPosix (round timestamp)) Json.Decode.float


eventEncoder : Event -> Json.Encode.Value
eventEncoder event =
    Json.Encode.object
        [ ( "timestamp", Json.Encode.int (Time.posixToMillis event.timestamp) )
        , ( "eventType", eventTypeEncoder event.eventType )
        , ( "clientId", Json.Encode.string event.clientId )
        , ( "isHidden", Json.Encode.bool event.isHidden )
        ]


eventTypeDecoder : Decoder EventType
eventTypeDecoder =
    Json.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\text ->
                case text of
                    "KeyDown" ->
                        Json.Decode.map KeyDown (Json.Decode.field "args" keyEventDecoder)

                    "KeyUp" ->
                        Json.Decode.map KeyUp (Json.Decode.field "args" keyEventDecoder)

                    "Click" ->
                        Json.Decode.map Click (Json.Decode.field "args" clickEventDecoder)

                    "ClickLink" ->
                        Json.Decode.map ClickLink (Json.Decode.field "args" linkEventDecoder)

                    "Http" ->
                        Json.Decode.map Http (Json.Decode.field "args" httpEventDecoder)

                    "HttpLocal" ->
                        Json.Decode.map HttpLocal (Json.Decode.field "args" httpLocalEventDecoder)

                    "Connect" ->
                        Json.Decode.map Connect (Json.Decode.field "args" connectEventDecoder)

                    "Paste" ->
                        Json.Decode.map Paste (Json.Decode.field "args" pasteEventDecoder)

                    "Input" ->
                        Json.Decode.map Input (Json.Decode.field "args" inputEventDecoder)

                    "FromJsPort" ->
                        Json.Decode.map FromJsPort (Json.Decode.field "args" fromJsPortEventDecoder)

                    "WindowResize" ->
                        Json.Decode.map WindowResize (Json.Decode.field "args" windowResizeEventDecoder)

                    "PointerDown" ->
                        Json.Decode.map PointerDown (Json.Decode.field "args" pointerEventDecoder)

                    "PointerUp" ->
                        Json.Decode.map PointerUp (Json.Decode.field "args" pointerEventDecoder)

                    "PointerMove" ->
                        Json.Decode.map PointerMove (Json.Decode.field "args" pointerEventDecoder)

                    "PointerLeave" ->
                        Json.Decode.map PointerLeave (Json.Decode.field "args" pointerEventDecoder)

                    "PointerCancel" ->
                        Json.Decode.map PointerCancel (Json.Decode.field "args" pointerEventDecoder)

                    "PointerOver" ->
                        Json.Decode.map PointerOver (Json.Decode.field "args" pointerEventDecoder)

                    "PointerEnter" ->
                        Json.Decode.map PointerEnter (Json.Decode.field "args" pointerEventDecoder)

                    "PointerOut" ->
                        Json.Decode.map PointerOut (Json.Decode.field "args" pointerEventDecoder)

                    "TouchStart" ->
                        Json.Decode.map TouchStart (Json.Decode.field "args" touchEventDecoder)

                    "TouchCancel" ->
                        Json.Decode.map TouchCancel (Json.Decode.field "args" touchEventDecoder)

                    "TouchMove" ->
                        Json.Decode.map TouchMove (Json.Decode.field "args" touchEventDecoder)

                    "TouchEnd" ->
                        Json.Decode.map TouchEnd (Json.Decode.field "args" touchEventDecoder)

                    "CheckView" ->
                        Json.Decode.map CheckView (Json.Decode.field "args" checkViewDecoder)

                    "MouseDown" ->
                        Json.Decode.map MouseDown (Json.Decode.field "args" mouseEventDecoder)

                    "MouseUp" ->
                        Json.Decode.map MouseUp (Json.Decode.field "args" mouseEventDecoder)

                    "MouseMove" ->
                        Json.Decode.map MouseMove (Json.Decode.field "args" mouseEventDecoder)

                    "MouseLeave" ->
                        Json.Decode.map MouseLeave (Json.Decode.field "args" mouseEventDecoder)

                    "MouseOver" ->
                        Json.Decode.map MouseOver (Json.Decode.field "args" mouseEventDecoder)

                    "MouseEnter" ->
                        Json.Decode.map MouseEnter (Json.Decode.field "args" mouseEventDecoder)

                    "MouseOut" ->
                        Json.Decode.map MouseOut (Json.Decode.field "args" mouseEventDecoder)

                    "Focus" ->
                        Json.Decode.map Focus (Json.Decode.field "args" focusEventDecoder)

                    "Blur" ->
                        Json.Decode.map Blur (Json.Decode.field "args" blurEventDecoder)

                    "Wheel" ->
                        Json.Decode.map Wheel (Json.Decode.field "args" wheelEventDecoder)

                    "Files" ->
                        Json.Decode.map File (Json.Decode.field "args" fileEventDecoder)

                    _ ->
                        Json.Decode.fail "Event not handled"
            )


eventTypeEncoder : EventType -> Json.Encode.Value
eventTypeEncoder eventType =
    case eventType of
        KeyDown keyEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "KeyDown" )
                , ( "args", keyEventEncoder keyEvent )
                ]

        KeyUp keyEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "KeyUp" )
                , ( "args", keyEventEncoder keyEvent )
                ]

        Click clickEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Click" )
                , ( "args", clickEventEncoder clickEvent )
                ]

        ClickLink linkEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ClickLink" )
                , ( "args", linkEventEncoder linkEvent )
                ]

        Http httpEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Http" )
                , ( "args", httpEventEncoder httpEvent )
                ]

        HttpLocal httpLocalEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "HttpLocal" )
                , ( "args", httpLocalEventEncoder httpLocalEvent )
                ]

        Connect connectEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Connect" )
                , ( "args", connectEventEncoder connectEvent )
                ]

        Paste pasteEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Paste" )
                , ( "args", pasteEventEncoder pasteEvent )
                ]

        Input inputEvent ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Input" )
                , ( "args", inputEventEncoder inputEvent )
                ]

        FromJsPort fromJsPort ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "FromJsPort" )
                , ( "args", fromJsPortEncoder fromJsPort )
                ]

        WindowResize windowResize ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "WindowResize" )
                , ( "args", windowResizeEncoder windowResize )
                ]

        PointerDown pointerDown ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "PointerDown" )
                , ( "args", pointerEventEncoder pointerDown )
                ]

        PointerUp pointerUp ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "PointerUp" )
                , ( "args", pointerEventEncoder pointerUp )
                ]

        PointerMove pointerMove ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "PointerMove" )
                , ( "args", pointerEventEncoder pointerMove )
                ]

        PointerLeave event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "PointerLeave" ), ( "args", pointerEventEncoder event ) ]

        PointerCancel event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "PointerCancel" ), ( "args", pointerEventEncoder event ) ]

        PointerOver event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "PointerOver" ), ( "args", pointerEventEncoder event ) ]

        PointerEnter event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "PointerEnter" ), ( "args", pointerEventEncoder event ) ]

        PointerOut event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "PointerOut" ), ( "args", pointerEventEncoder event ) ]

        TouchStart event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "TouchStart" ), ( "args", touchEventEncoder event ) ]

        TouchCancel event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "TouchCancel" ), ( "args", touchEventEncoder event ) ]

        TouchMove event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "TouchMove" ), ( "args", touchEventEncoder event ) ]

        TouchEnd event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "TouchEnd" ), ( "args", touchEventEncoder event ) ]

        CheckView event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "CheckView" ), ( "args", checkViewEncoder event ) ]

        MouseDown event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "MouseDown" ), ( "args", mouseEventEncoder event ) ]

        MouseUp event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "MouseUp" ), ( "args", mouseEventEncoder event ) ]

        MouseMove event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "MouseMove" ), ( "args", mouseEventEncoder event ) ]

        MouseLeave event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "MouseLeave" ), ( "args", mouseEventEncoder event ) ]

        MouseOver event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "MouseOver" ), ( "args", mouseEventEncoder event ) ]

        MouseEnter event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "MouseEnter" ), ( "args", mouseEventEncoder event ) ]

        MouseOut event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "MouseOut" ), ( "args", mouseEventEncoder event ) ]

        Focus event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Focus" ), ( "args", focusEventEncoder event ) ]

        Blur event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Blur" ), ( "args", blurEventEncoder event ) ]

        Wheel event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Wheel" ), ( "args", wheelEventEncoder event ) ]

        File event ->
            Json.Encode.object [ ( "tag", Json.Encode.string "File" ), ( "args", fileEventEncoder event ) ]


wheelEventDecoder : Decoder WheelEvent
wheelEventDecoder =
    Json.succeed WheelEvent
        |> andMap "deltaX" Json.Decode.float
        |> andMap "deltaY" Json.Decode.float
        |> andMap "deltaZ" Json.Decode.float
        |> andMap "deltaMode" Json.Decode.int
        |> andMap "mouseEvent" mouseEventDecoder


wheelEventEncoder : WheelEvent -> Json.Encode.Value
wheelEventEncoder event =
    Json.Encode.object
        [ ( "deltaX", Json.Encode.float event.deltaX )
        , ( "deltaY", Json.Encode.float event.deltaY )
        , ( "deltaZ", Json.Encode.float event.deltaZ )
        , ( "deltaMode", Json.Encode.int event.deltaMode )
        , ( "mouseEvent", mouseEventEncoder event.mouseEvent )
        ]


fileEventDecoder : Decoder FileEvent
fileEventDecoder =
    Json.succeed FileEvent
        |> andMap "files" (Json.Decode.list uploadedFileDecoder)
        |> andMap "allowMultiple" Json.Decode.bool


fileEventEncoder : FileEvent -> Json.Encode.Value
fileEventEncoder event =
    Json.Encode.object
        [ ( "files", Json.Encode.list uploadedFileEncoder event.files )
        , ( "allowMultiple", Json.Encode.bool event.allowMultiple )
        ]


uploadedFileDecoder : Decoder UploadedFile
uploadedFileDecoder =
    Json.succeed UploadedFile
        |> andMap "name" Json.Decode.string
        |> andMap "lastModified" Json.Decode.int
        |> andMap "contentFilePath" Json.Decode.string
        |> andMap "mimeType" Json.Decode.string


uploadedFileEncoder : UploadedFile -> Json.Encode.Value
uploadedFileEncoder event =
    Json.Encode.object
        [ ( "name", Json.Encode.string event.name )
        , ( "lastModified", Json.Encode.int event.lastModified )
        , ( "contentFilePath", Json.Encode.string event.contentFilePath )
        , ( "mimeType", Json.Encode.string event.mimeType )
        ]


focusEventDecoder : Decoder FocusEvent
focusEventDecoder =
    Json.succeed FocusEvent |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)


maybeEncoder : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
maybeEncoder encoder maybe =
    case maybe of
        Just a ->
            encoder a

        Nothing ->
            Json.Encode.null


focusEventEncoder : FocusEvent -> Json.Encode.Value
focusEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        ]


blurEventDecoder : Decoder BlurEvent
blurEventDecoder =
    Json.succeed BlurEvent |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)


blurEventEncoder : BlurEvent -> Json.Encode.Value
blurEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        ]


mouseEventDecoder : Decoder MouseEvent
mouseEventDecoder =
    Json.succeed MouseEvent
        |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)
        |> andMap "ctrlKey" Json.Decode.bool
        |> andMap "shiftKey" Json.Decode.bool
        |> andMap "metaKey" Json.Decode.bool
        |> andMap "altKey" Json.Decode.bool
        |> andMap "clientX" Json.Decode.float
        |> andMap "clientY" Json.Decode.float
        |> andMap "offsetX" Json.Decode.float
        |> andMap "offsetY" Json.Decode.float
        |> andMap "pageX" Json.Decode.float
        |> andMap "pageY" Json.Decode.float
        |> andMap "screenX" Json.Decode.float
        |> andMap "screenY" Json.Decode.float
        |> andMap "button" Json.Decode.int


mouseEventEncoder : MouseEvent -> Json.Encode.Value
mouseEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        , ( "ctrlKey", Json.Encode.bool event.ctrlKey )
        , ( "shiftKey", Json.Encode.bool event.shiftKey )
        , ( "metaKey", Json.Encode.bool event.metaKey )
        , ( "altKey", Json.Encode.bool event.altKey )
        , ( "clientX", Json.Encode.float event.clientX )
        , ( "clientY", Json.Encode.float event.clientY )
        , ( "offsetX", Json.Encode.float event.offsetX )
        , ( "offsetY", Json.Encode.float event.offsetY )
        , ( "pageX", Json.Encode.float event.pageX )
        , ( "pageY", Json.Encode.float event.pageY )
        , ( "screenX", Json.Encode.float event.screenX )
        , ( "screenY", Json.Encode.float event.screenY )
        , ( "button", Json.Encode.int event.button )
        ]


checkViewDecoder : Decoder CheckViewEvent
checkViewDecoder =
    Json.succeed CheckViewEvent
        |> andMap "selection" (Json.Decode.list Json.Decode.string)


checkViewEncoder : CheckViewEvent -> Json.Encode.Value
checkViewEncoder event =
    Json.Encode.object
        [ ( "selection", Json.Encode.list Json.Encode.string event.selection )
        ]


pointerEventDecoder : Decoder PointerEvent
pointerEventDecoder =
    Json.succeed PointerEvent
        |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)
        |> andMap "ctrlKey" Json.Decode.bool
        |> andMap "shiftKey" Json.Decode.bool
        |> andMap "metaKey" Json.Decode.bool
        |> andMap "altKey" Json.Decode.bool
        |> andMap "clientX" Json.Decode.float
        |> andMap "clientY" Json.Decode.float
        |> andMap "offsetX" Json.Decode.float
        |> andMap "offsetY" Json.Decode.float
        |> andMap "pageX" Json.Decode.float
        |> andMap "pageY" Json.Decode.float
        |> andMap "screenX" Json.Decode.float
        |> andMap "screenY" Json.Decode.float
        |> andMap "button" Json.Decode.int
        |> andMap "pointerType" Json.Decode.string
        |> andMap "pointerId" Json.Decode.int
        |> andMap "isPrimary" Json.Decode.bool
        |> andMap "width" Json.Decode.float
        |> andMap "height" Json.Decode.float
        |> andMap "pressure" Json.Decode.float
        |> andMap "tiltX" Json.Decode.float
        |> andMap "tiltY" Json.Decode.float


pointerEventEncoder : PointerEvent -> Json.Encode.Value
pointerEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        , ( "ctrlKey", Json.Encode.bool event.ctrlKey )
        , ( "shiftKey", Json.Encode.bool event.shiftKey )
        , ( "metaKey", Json.Encode.bool event.metaKey )
        , ( "altKey", Json.Encode.bool event.altKey )
        , ( "clientX", Json.Encode.float event.clientX )
        , ( "clientY", Json.Encode.float event.clientY )
        , ( "offsetX", Json.Encode.float event.offsetX )
        , ( "offsetY", Json.Encode.float event.offsetY )
        , ( "pageX", Json.Encode.float event.pageX )
        , ( "pageY", Json.Encode.float event.pageY )
        , ( "screenX", Json.Encode.float event.screenX )
        , ( "screenY", Json.Encode.float event.screenY )
        , ( "button", Json.Encode.int event.button )
        , ( "pointerType", Json.Encode.string event.pointerType )
        , ( "pointerId", Json.Encode.int event.pointerId )
        , ( "isPrimary", Json.Encode.bool event.isPrimary )
        , ( "width", Json.Encode.float event.width )
        , ( "height", Json.Encode.float event.height )
        , ( "pressure", Json.Encode.float event.pressure )
        , ( "tiltX", Json.Encode.float event.tiltX )
        , ( "tiltY", Json.Encode.float event.tiltY )
        ]


touchEventDecoder : Decoder TouchEvent
touchEventDecoder =
    Json.succeed TouchEvent
        |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)
        |> andMap "ctrlKey" Json.Decode.bool
        |> andMap "shiftKey" Json.Decode.bool
        |> andMap "metaKey" Json.Decode.bool
        |> andMap "altKey" Json.Decode.bool
        |> andMap "changedTouches" (Json.Decode.list touchDecoder)
        |> andMap "targetTouches" (Json.Decode.list touchDecoder)
        |> andMap "touches" (Json.Decode.list touchDecoder)


touchEventEncoder : TouchEvent -> Json.Encode.Value
touchEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        , ( "ctrlKey", Json.Encode.bool event.ctrlKey )
        , ( "shiftKey", Json.Encode.bool event.shiftKey )
        , ( "metaKey", Json.Encode.bool event.metaKey )
        , ( "altKey", Json.Encode.bool event.altKey )
        , ( "changedTouches"
          , Json.Encode.list touchEncoder event.changedTouches
          )
        , ( "targetTouches"
          , Json.Encode.list touchEncoder event.targetTouches
          )
        , ( "touches", Json.Encode.list touchEncoder event.touches )
        ]


touchDecoder : Decoder Touch
touchDecoder =
    Json.succeed Touch
        |> andMap "clientX" Json.Decode.float
        |> andMap "clientY" Json.Decode.float
        |> andMap "pageX" Json.Decode.float
        |> andMap "pageY" Json.Decode.float
        |> andMap "screenX" Json.Decode.float
        |> andMap "screenY" Json.Decode.float
        |> andMap "identifier" Json.Decode.int


touchEncoder : Touch -> Json.Encode.Value
touchEncoder event =
    Json.Encode.object
        [ ( "clientX", Json.Encode.float event.clientX )
        , ( "clientY", Json.Encode.float event.clientY )
        , ( "pageX", Json.Encode.float event.pageX )
        , ( "pageY", Json.Encode.float event.pageY )
        , ( "screenX", Json.Encode.float event.screenX )
        , ( "screenY", Json.Encode.float event.screenY )
        , ( "identifier", Json.Encode.int event.identifier )
        ]


connectEventDecoder : Decoder ConnectEvent
connectEventDecoder =
    Json.succeed ConnectEvent
        |> andMap "url" Json.Decode.string
        |> andMap "sessionId" Json.Decode.string
        |> andMap "windowWidth" Json.Decode.int
        |> andMap "windowHeight" Json.Decode.int


connectEventEncoder : ConnectEvent -> Json.Encode.Value
connectEventEncoder event =
    Json.Encode.object
        [ ( "url", Json.Encode.string event.url )
        , ( "sessionId", Json.Encode.string event.sessionId )
        , ( "windowWidth", Json.Encode.int event.windowWidth )
        , ( "windowHeight", Json.Encode.int event.windowHeight )
        ]


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    Json.succeed KeyEvent
        |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)
        |> andMap "ctrlKey" Json.Decode.bool
        |> andMap "shiftKey" Json.Decode.bool
        |> andMap "metaKey" Json.Decode.bool
        |> andMap "altKey" Json.Decode.bool
        |> andMap "key" Json.Decode.string


keyEventEncoder : KeyEvent -> Json.Encode.Value
keyEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        , ( "ctrlKey", Json.Encode.bool event.ctrlKey )
        , ( "shiftKey", Json.Encode.bool event.shiftKey )
        , ( "metaKey", Json.Encode.bool event.metaKey )
        , ( "altKey", Json.Encode.bool event.altKey )
        , ( "key", Json.Encode.string event.key )
        ]


clickEventDecoder : Decoder ClickEvent
clickEventDecoder =
    Json.succeed ClickEvent
        |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)


clickEventEncoder : ClickEvent -> Json.Encode.Value
clickEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        ]


pasteEventDecoder : Decoder PasteEvent
pasteEventDecoder =
    Json.succeed PasteEvent
        |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)
        |> andMap "text" Json.Decode.string


pasteEventEncoder : PasteEvent -> Json.Encode.Value
pasteEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        , ( "text", Json.Encode.string event.text )
        ]


inputEventDecoder : Decoder InputEvent
inputEventDecoder =
    Json.succeed InputEvent
        |> andMap "targetId" (Json.Decode.nullable Json.Decode.string)
        |> andMap "text" Json.Decode.string


inputEventEncoder : InputEvent -> Json.Encode.Value
inputEventEncoder event =
    Json.Encode.object
        [ ( "targetId"
          , maybeEncoder Json.Encode.string event.targetId
          )
        , ( "text", Json.Encode.string event.text )
        ]


fromJsPortEventDecoder : Decoder FromJsPortEvent
fromJsPortEventDecoder =
    Json.succeed FromJsPortEvent
        |> andMap "port" Json.Decode.string
        |> andMap "data" Json.Decode.string


fromJsPortEncoder : FromJsPortEvent -> Json.Encode.Value
fromJsPortEncoder event =
    Json.Encode.object
        [ ( "port", Json.Encode.string event.port_ )
        , ( "data", Json.Encode.string event.data )
        ]


httpLocalEventDecoder : Decoder HttpLocalEvent
httpLocalEventDecoder =
    Json.succeed HttpLocalEvent
        |> andMap "filepath" Json.Decode.string


httpLocalEventEncoder : HttpLocalEvent -> Json.Encode.Value
httpLocalEventEncoder event =
    Json.Encode.object
        [ ( "filepath", Json.Encode.string event.filepath )
        ]


windowResizeEventDecoder : Decoder WindowResizeEvent
windowResizeEventDecoder =
    Json.succeed WindowResizeEvent
        |> andMap "width" Json.Decode.int
        |> andMap "height" Json.Decode.int


windowResizeEncoder : WindowResizeEvent -> Json.Encode.Value
windowResizeEncoder event =
    Json.Encode.object
        [ ( "width", Json.Encode.int event.width )
        , ( "height", Json.Encode.int event.height )
        ]


linkEventDecoder : Decoder LinkEvent
linkEventDecoder =
    Json.succeed LinkEvent
        |> andMap "path" Json.Decode.string


linkEventEncoder : LinkEvent -> Json.Encode.Value
linkEventEncoder event =
    Json.Encode.object
        [ ( "path", Json.Encode.string event.path )
        ]


httpEventDecoder : Decoder HttpEvent
httpEventDecoder =
    Json.succeed HttpEvent
        |> andMap "responseType" Json.Decode.string
        |> andMap "method" Json.Decode.string
        |> andMap "url" Json.Decode.string
        |> andMap "filepath" Json.Decode.string


httpEventEncoder : HttpEvent -> Json.Encode.Value
httpEventEncoder event =
    Json.Encode.object
        [ ( "responseType", Json.Encode.string event.responseType )
        , ( "method", Json.Encode.string event.method )
        , ( "url", Json.Encode.string event.url )
        , ( "filepath", Json.Encode.string event.filepath )
        ]


andMap : String -> Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
andMap field a b =
    Json.map2 (|>) (Json.field field a) b


type alias LoadedData =
    { copyCounter : Int
    , settings : Settings
    , parsedCode : ParsedCodeStatus
    , mouseDownOnEvent : Bool
    , commitStatus : CommitStatus
    }


type CommitStatus
    = NotCommitted
    | Committing String
    | CommitFailed


type alias Settings =
    { includeClientPos : Bool
    , includePagePos : Bool
    , includeScreenPos : Bool
    , showAllCode : Bool
    }


type ParsedCodeStatus
    = ParseSuccess ParsedCode
    | ParseFailed ParseError
    | WaitingOnFile


type ParseError
    = InvalidFileRequests
    | FileRequestsNotFound
    | TestEntryPointNotFound
    | FileRequestsEndNotFound
    | UnknownError


type alias ParsedCode =
    { codeParts : List Code
    , fileRequests : List ( String, String )
    , noPriorTests : Bool
    , testName : String
    }


type Code
    = UserCode String
    | FileRequestCode
    | TestEntryPoint


type alias Session =
    { history : Array Event
    , connections : Set ClientId
    , settings : Settings
    }


type TestEditorMsg
    = MouseDownOnEvent Int Bool
    | MouseUpEvent
    | MouseEnterOnEvent Int Bool
    | ToggledIncludeScreenPos Bool
    | ToggledIncludeClientPos Bool
    | ToggledIncludePagePos Bool
    | PressedEvent
    | GotTestFile (Result Http.Error String)
    | PressedSaveTest { shouldOpen : Bool }
    | PressedDeleteTest
    | ToggledShowAllCode Bool
    | WroteToFile { shouldOpen : Maybe String } (Result Http.Error ())


type alias Event =
    { isHidden : Bool
    , clientId : ClientId
    , timestamp : Time.Posix
    , eventType : EventType
    }


type EventType
    = KeyDown KeyEvent
    | KeyUp KeyEvent
    | Click ClickEvent
    | ClickLink LinkEvent
    | Http HttpEvent
    | HttpLocal HttpLocalEvent
    | Connect ConnectEvent
    | Paste PasteEvent
    | Input InputEvent
    | FromJsPort FromJsPortEvent
    | WindowResize WindowResizeEvent
    | PointerDown PointerEvent
    | PointerUp PointerEvent
    | PointerMove PointerEvent
    | PointerLeave PointerEvent
    | PointerCancel PointerEvent
    | PointerOver PointerEvent
    | PointerEnter PointerEvent
    | PointerOut PointerEvent
    | TouchStart TouchEvent
    | TouchCancel TouchEvent
    | TouchMove TouchEvent
    | TouchEnd TouchEvent
    | CheckView CheckViewEvent
    | MouseDown MouseEvent
    | MouseUp MouseEvent
    | MouseMove MouseEvent
    | MouseLeave MouseEvent
    | MouseOver MouseEvent
    | MouseEnter MouseEvent
    | MouseOut MouseEvent
    | Focus FocusEvent
    | Blur BlurEvent
    | Wheel WheelEvent
    | File FileEvent


type alias FileEvent =
    { files : List UploadedFile, allowMultiple : Bool }


type alias UploadedFile =
    { name : String
    , lastModified : Int
    , contentFilePath : String
    , mimeType : String
    }


type alias FocusEvent =
    { targetId : Maybe String }


type alias BlurEvent =
    { targetId : Maybe String }


type alias CheckViewEvent =
    { selection : List String
    }


type alias PointerEvent =
    { targetId : Maybe String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , button : Int
    , pointerType : String
    , pointerId : Int
    , isPrimary : Bool
    , width : Float
    , height : Float
    , pressure : Float
    , tiltX : Float
    , tiltY : Float
    }


type alias MouseEvent =
    { targetId : Maybe String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , button : Int
    }


type alias WheelEvent =
    { deltaX : Float
    , deltaY : Float
    , deltaZ : Float
    , deltaMode : Int
    , mouseEvent : MouseEvent
    }


type alias TouchEvent =
    { targetId : Maybe String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , changedTouches : List Touch
    , targetTouches : List Touch
    , touches : List Touch
    }


type alias Touch =
    { clientX : Float
    , clientY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , identifier : Int
    }


type alias ConnectEvent =
    { url : String, sessionId : SessionId, windowWidth : Int, windowHeight : Int }


type alias WindowResizeEvent =
    { width : Int, height : Int }


type alias HttpLocalEvent =
    { filepath : String }


type alias FromJsPortEvent =
    { port_ : String, data : String }


type alias InputEvent =
    { targetId : Maybe String, text : String }


type alias KeyEvent =
    { targetId : Maybe String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , key : String
    }


type alias ClickEvent =
    { targetId : Maybe String
    }


type alias PasteEvent =
    { targetId : Maybe String
    , text : String
    }


type alias LinkEvent =
    { path : String
    }


type alias HttpEvent =
    { responseType : String
    , method : String
    , url : String
    , filepath : String
    }


sansSerifFont =
    Html.Attributes.style "font-family" "sans-serif"


backgroundColor config =
    Html.Attributes.style "background-color" config.charcoal


fontColor config =
    Html.Attributes.style "color" config.white


testEditorView : ViewConfig -> RecordingState -> LoadedData -> Html TestEditorMsg
testEditorView config recording model =
    case model.parsedCode of
        WaitingOnFile ->
            Html.div
                [ Html.Attributes.style "height" "100vh", backgroundColor config ]
                [ Html.div
                    [ Html.Attributes.style "padding" "16px"
                    , sansSerifFont
                    , fontColor config
                    ]
                    [ Html.text "Loading tests..." ]
                ]

        ParseSuccess parsed ->
            let
                eventsList : List Event
                eventsList =
                    Array.toList recording.history

                button : msg -> String -> Html msg
                button onPress text =
                    Html.button
                        [ Html.Attributes.style "border" "1px solid #787c7c"
                        , Html.Attributes.style "background-color" "#f0ebee"
                        , Html.Attributes.style "flex-grow" "1"
                        , Html.Attributes.style "padding" "0px 10px"
                        , Html.Events.onClick onPress
                        ]
                        [ Html.text text
                        ]
            in
            Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "width" "100vw"
                , Html.Attributes.style "height" "100vh"
                , Html.Attributes.style "overflow" "auto"
                , sansSerifFont
                , backgroundColor config
                , fontColor config
                ]
                [ Html.div
                    [ Html.Attributes.style "width" "300px"
                    , Html.Attributes.style "min-width" "300px"
                    , Html.Attributes.style "background-color" lightCharcoal
                    , Html.Attributes.style "border-right" "1px solid #787c7c"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    ]
                    [ Html.b
                        [ Html.Attributes.style "padding" "8px"
                        , Html.Attributes.style "border-color" "#787c7c"
                        , Html.Attributes.style "border-style" "solid"
                        , Html.Attributes.style "border-width" "0 0 1px 0"
                        ]
                        [ Html.text "Recorded events" ]
                    , eventsView config eventsList
                    ]
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-grow" "1"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "overflow" "auto"
                    ]
                    [ Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-grow" "1"
                        ]
                        [ Html.div
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "flex-direction" "column"
                            , Html.Attributes.style "flex-grow" "1"
                            ]
                            [ simpleCheckbox ToggledIncludeClientPos "Include clientPos in pointer events" model.settings.includeClientPos
                            , simpleCheckbox ToggledIncludePagePos "Include pagePos in pointer events" model.settings.includePagePos
                            , simpleCheckbox ToggledIncludeScreenPos "Include screenPos in pointer events" model.settings.includeScreenPos
                            , simpleCheckbox ToggledShowAllCode "Show all generated code" model.settings.showAllCode
                            ]
                        , Html.div
                            [ Html.Attributes.style "border-bottom" "1px solid #787c7c"
                            , Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "flex-direction" "column"
                            ]
                            [ button PressedDeleteTest "Delete test"
                            , button (PressedSaveTest { shouldOpen = False }) "Save test"
                            , button (PressedSaveTest { shouldOpen = True }) "Save and open test"
                            ]
                        ]
                    , Html.div
                        [ Html.Attributes.style "overflow" "auto"
                        , Html.Attributes.style "padding" "8px"
                        , Html.Attributes.style "white-space" "pre"
                        , Html.Attributes.style "font-family" "monospace"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "border-color" "#787c7c"
                        , Html.Attributes.style "border-style" "solid"
                        , Html.Attributes.style "border-width" "1px 0 0 0"
                        ]
                        [ codegen
                            parsed
                            model.settings
                            (List.filter (\event -> not event.isHidden) eventsList)
                            |> Html.text
                        ]
                    ]
                ]

        ParseFailed error ->
            Html.div
                [ Html.Attributes.style "height" "100vh", backgroundColor config ]
                [ Html.div
                    [ Html.Attributes.style "padding" "16px"
                    , sansSerifFont
                    , fontColor config
                    , Html.Attributes.style "display" "flex"
                    ]
                    [ Html.div
                        [ Html.Attributes.style "flex-grow" "1" ]
                        [ parseErrorToString error |> Html.text ]
                    , Html.div [ Html.Attributes.style "width" "32px" ] []
                    , Html.button
                        [ Html.Attributes.style "border" "1px solid #787c7c"
                        , Html.Attributes.style "background-color" "#f0ebee"
                        , Html.Attributes.style "padding" "0px 10px"
                        , Html.Attributes.style "height" "40px"
                        , Html.Events.onClick PressedDeleteTest
                        ]
                        [ Html.text "Close"
                        ]
                    ]
                ]



--(case model.parsedCode of
--    ParseSuccess _ ->
--        [ Ui.el
--            [ Ui.borderWith { left = 0, top = 1, bottom = 0, right = 0 }
--            , Ui.borderColor (Ui.rgb 100 100 100)
--            , Ui.inFront
--                (Ui.row
--                    [ Ui.Input.button PressedCopyCode
--                    , Ui.border 1
--                    , Ui.borderColor (Ui.rgb 100 100 100)
--                    , Ui.background (Ui.rgb 240 240 240)
--                    , Ui.width Ui.shrink
--                    , Ui.padding 4
--                    , Ui.roundedWith { topLeft = 0, topRight = 0, bottomRight = 0, bottomLeft = 4 }
--                    , Ui.alignRight
--                    , Ui.move { x = 0, y = -1, z = 0 }
--                    , Ui.Font.size 14
--                    , Ui.spacing 4
--                    ]
--                    [ Icons.copy
--                    , if model.copyCounter > 0 then
--                        Ui.text "Copied!"
--
--                      else
--                        Ui.text "Copy to clipboard"
--                    ]
--                )
--            ]
--            Ui.none
--        ]
--
--    _ ->
--        []
--)


parseErrorToString : ParseError -> String
parseErrorToString error =
    case error of
        InvalidFileRequests ->
            "The fileRequests function was found but couldn't be parsed."

        FileRequestsNotFound ->
            "The fileRequests function wasn't found. If you deleted it by accident, revert the change. Otherwise delete " ++ testPath ++ " so it can be regenerated."

        TestEntryPointNotFound ->
            "The test entry point (the \"]\" at the end of the the tests function) wasn't found. If you removed it by accident, revert the change. Otherwise delete " ++ testPath ++ " so it can be regenerated."

        UnknownError ->
            testPath ++ " failed to load. Make sure you're running lamdera live with EXPERIMENTAL=1"

        FileRequestsEndNotFound ->
            "The fileRequests function was found but it's supposed to end with \"|> Dict.fromList\". If you removed it by accident, revert the change. Otherwise delete " ++ testPath ++ " so it can be regenerated."


simpleCheckbox : (Bool -> msg) -> String -> Bool -> Html msg
simpleCheckbox msg text value =
    Html.div
        [ Html.Attributes.style "padding" "2px" ]
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked value
            , Html.Events.onCheck msg
            , Html.Attributes.style "font-size" "14px"
            , Html.Attributes.name text
            , Html.Attributes.id text
            ]
            []
        , Html.label [ Html.Attributes.for text ] [ Html.text text ]
        ]


eventsView : ViewConfig -> List Event -> Html TestEditorMsg
eventsView config events =
    case events of
        [] ->
            Html.i [ Html.Attributes.style "padding" "16px" ] [ Html.text "No events were recorded" ]

        _ ->
            List.indexedMap (\index a -> ( index, a )) events
                |> List.filterMap
                    (\( index, event ) ->
                        let
                            maybeText : Maybe String
                            maybeText =
                                case event.eventType of
                                    KeyDown keyEvent ->
                                        keyEvent.key ++ " key down" |> Just

                                    KeyUp keyEvent ->
                                        keyEvent.key ++ " key up" |> Just

                                    Click mouseEvent ->
                                        case mouseEvent.targetId of
                                            Just id ->
                                                "Click " ++ id |> Just

                                            Nothing ->
                                                "Click but Html.Attributes.id is missing" |> Just

                                    Http _ ->
                                        Nothing

                                    Connect record ->
                                        "Connected " ++ record.sessionId |> Just

                                    ClickLink linkEvent ->
                                        "Click link " ++ linkEvent.path |> Just

                                    Paste pasteEvent ->
                                        "Pasted text " ++ pasteEvent.text |> Just

                                    Input inputEvent ->
                                        "Text input " ++ inputEvent.text |> Just

                                    FromJsPort fromJsPortEvent ->
                                        "Port "
                                            ++ fromJsPortEvent.port_
                                            ++ " "
                                            ++ fromJsPortEvent.data
                                            |> Just

                                    HttpLocal { filepath } ->
                                        Nothing

                                    WindowResize { width, height } ->
                                        "Window resized w:" ++ String.fromInt width ++ " h:" ++ String.fromInt height |> Just

                                    PointerDown _ ->
                                        "Pointer down" |> Just

                                    PointerUp _ ->
                                        "Pointer up" |> Just

                                    PointerMove _ ->
                                        "Pointer move" |> Just

                                    PointerLeave _ ->
                                        "Pointer leave" |> Just

                                    PointerCancel _ ->
                                        "Pointer cancel" |> Just

                                    PointerOver _ ->
                                        "Pointer over" |> Just

                                    PointerEnter _ ->
                                        "Pointer enter" |> Just

                                    PointerOut _ ->
                                        "Pointer out" |> Just

                                    TouchStart _ ->
                                        "Touch start" |> Just

                                    TouchCancel _ ->
                                        "Touch cancel" |> Just

                                    TouchMove _ ->
                                        "Touch move" |> Just

                                    TouchEnd _ ->
                                        "Touch end" |> Just

                                    CheckView _ ->
                                        "Check view" |> Just

                                    MouseDown _ ->
                                        "Mouse down" |> Just

                                    MouseUp _ ->
                                        "Mouse up" |> Just

                                    MouseMove _ ->
                                        "Mouse move" |> Just

                                    MouseLeave _ ->
                                        "Mouse leave" |> Just

                                    MouseOver _ ->
                                        "Mouse over" |> Just

                                    MouseEnter _ ->
                                        "Mouse enter" |> Just

                                    MouseOut _ ->
                                        "Mouse out" |> Just

                                    Focus _ ->
                                        "Focus" |> Just

                                    Blur _ ->
                                        "Blur" |> Just

                                    Wheel _ ->
                                        "Mouse wheel scrolled" |> Just

                                    File _ ->
                                        Nothing
                        in
                        case maybeText of
                            Just text ->
                                eventButton config text index event |> Just

                            Nothing ->
                                Nothing
                    )
                |> Html.div
                    [ Html.Attributes.style "font-size" "14px"
                    , Html.Attributes.style "padding" "8px 0"
                    , Html.Attributes.style "overflow" "clip auto"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "align-items" "start"
                    ]


eventButton : ViewConfig -> String -> Int -> Event -> Html TestEditorMsg
eventButton config text index event =
    Html.button
        [ Html.Attributes.style "border" "none"
        , Html.Attributes.style "background-color" "transparent"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "overflow" "clip"
        , Html.Attributes.style "width" "300px"
        , Html.Attributes.style "white-space" "pre"
        , Html.Events.onClick PressedEvent
        , Html.Events.onMouseDown (MouseDownOnEvent index (not event.isHidden))
        , Html.Events.onMouseEnter (MouseEnterOnEvent index (not event.isHidden))
        , Html.Attributes.style "padding" "4px 8px"
        , Html.Attributes.style "color"
            (if event.isHidden then
                "#787c7c"

             else
                config.white
            )
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "start"
        , Html.Attributes.title text
        ]
        [ Html.span
            [ Html.Attributes.style "padding-right" "4px" ]
            [ if event.isHidden then
                eyeClosed

              else
                eye
            ]
        , Html.text text
        ]


type alias MillisecondWaitBefore =
    Int


type EventType2
    = Input2 ClientId MillisecondWaitBefore { targetId : Maybe String, text : String }
    | Click2 ClientId MillisecondWaitBefore { targetId : Maybe String }
    | ClickLink2 ClientId MillisecondWaitBefore LinkEvent
    | Connect2 ClientId MillisecondWaitBefore ConnectEvent (List EventType2)
    | KeyUp2 ClientId MillisecondWaitBefore KeyEvent
    | KeyDown2 ClientId MillisecondWaitBefore KeyEvent
    | PointerDown2 ClientId MillisecondWaitBefore PointerEvent
    | PointerUp2 ClientId MillisecondWaitBefore PointerEvent
    | PointerMove2 ClientId MillisecondWaitBefore PointerEvent
    | PointerLeave2 ClientId MillisecondWaitBefore PointerEvent
    | PointerCancel2 ClientId MillisecondWaitBefore PointerEvent
    | PointerOver2 ClientId MillisecondWaitBefore PointerEvent
    | PointerEnter2 ClientId MillisecondWaitBefore PointerEvent
    | PointerOut2 ClientId MillisecondWaitBefore PointerEvent
    | TouchStart2 ClientId MillisecondWaitBefore TouchEvent
    | TouchCancel2 ClientId MillisecondWaitBefore TouchEvent
    | TouchMove2 ClientId MillisecondWaitBefore TouchEvent
    | TouchEnd2 ClientId MillisecondWaitBefore TouchEvent
    | FromJsPort2 ClientId MillisecondWaitBefore { port_ : String, data : String }
    | WindowResize2 ClientId MillisecondWaitBefore WindowResizeEvent
    | CheckView2 ClientId MillisecondWaitBefore CheckViewEvent
    | MouseDown2 ClientId MillisecondWaitBefore MouseEvent
    | MouseUp2 ClientId MillisecondWaitBefore MouseEvent
    | MouseMove2 ClientId MillisecondWaitBefore MouseEvent
    | MouseLeave2 ClientId MillisecondWaitBefore MouseEvent
    | MouseOver2 ClientId MillisecondWaitBefore MouseEvent
    | MouseEnter2 ClientId MillisecondWaitBefore MouseEvent
    | MouseOut2 ClientId MillisecondWaitBefore MouseEvent
    | Focus2 ClientId MillisecondWaitBefore FocusEvent
    | Blur2 ClientId MillisecondWaitBefore BlurEvent
    | Wheel2 ClientId MillisecondWaitBefore WheelEvent


eventsToEvent2Helper state =
    maybeToList state.previousEvent
        ++ state.rest
        |> List.reverse


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just a ->
            [ a ]

        Nothing ->
            []


eventsToEvent2 :
    { previousEvent : Maybe EventType2, previousTime : Int, rest : List EventType2 }
    -> List Event
    -> List EventType2
eventsToEvent2 ({ previousEvent, previousTime, rest } as state) events =
    case events of
        [] ->
            eventsToEvent2Helper { previousEvent = previousEvent, rest = rest }

        { clientId, eventType, timestamp } :: events2 ->
            let
                timestamp2 =
                    Time.posixToMillis timestamp

                delay2 : Int
                delay2 =
                    timestamp2 - previousTime
            in
            case eventType of
                Input input ->
                    eventsToEvent2
                        { previousEvent = Just (Input2 clientId delay2 input)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                KeyDown keyDown ->
                    eventsToEvent2
                        { previousEvent = Just (KeyDown2 clientId delay2 keyDown)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                KeyUp keyUp ->
                    eventsToEvent2
                        { previousEvent = Just (KeyUp2 clientId delay2 keyUp)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerDown a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerDown2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerUp a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerUp2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerMove a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerMove2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerLeave a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerLeave2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerCancel a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerCancel2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerOver a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerOver2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerEnter a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerEnter2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                PointerOut a ->
                    eventsToEvent2
                        { previousEvent = Just (PointerOut2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                TouchStart a ->
                    eventsToEvent2
                        { previousEvent = Just (TouchStart2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                TouchCancel a ->
                    eventsToEvent2
                        { previousEvent = Just (TouchCancel2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                TouchMove a ->
                    eventsToEvent2
                        { previousEvent = Just (TouchMove2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                TouchEnd a ->
                    eventsToEvent2
                        { previousEvent = Just (TouchEnd2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                Connect connect ->
                    { previousEvent =
                        Just
                            (Connect2
                                clientId
                                delay2
                                connect
                                (eventsToEvent2
                                    { previousEvent = Nothing
                                    , previousTime = timestamp2
                                    , rest = []
                                    }
                                    events2
                                )
                            )
                    , rest = maybeToList previousEvent ++ rest
                    }
                        |> eventsToEvent2Helper

                Click mouseEvent ->
                    eventsToEvent2
                        { previousEvent = Just (Click2 clientId delay2 { targetId = mouseEvent.targetId })
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                ClickLink linkEvent ->
                    eventsToEvent2
                        { previousEvent = Just (ClickLink2 clientId delay2 linkEvent)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                Http _ ->
                    eventsToEvent2 state events2

                HttpLocal _ ->
                    eventsToEvent2 state events2

                Paste pasteEvent ->
                    eventsToEvent2
                        { previousEvent =
                            Just
                                (Input2
                                    clientId
                                    delay2
                                    { targetId = pasteEvent.targetId
                                    , text = pasteEvent.text
                                    }
                                )
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                FromJsPort fromJsPort ->
                    eventsToEvent2
                        { previousEvent = Just (FromJsPort2 clientId delay2 { port_ = fromJsPort.port_, data = fromJsPort.data })
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                WindowResize resizeEvent ->
                    eventsToEvent2
                        { previousEvent = Just (WindowResize2 clientId delay2 resizeEvent)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                CheckView checkView ->
                    eventsToEvent2
                        { previousEvent = Just (CheckView2 clientId delay2 checkView)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                MouseDown a ->
                    eventsToEvent2
                        { previousEvent = Just (MouseDown2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                MouseUp a ->
                    eventsToEvent2
                        { previousEvent = Just (MouseUp2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                MouseMove a ->
                    eventsToEvent2
                        { previousEvent = Just (MouseMove2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                MouseLeave a ->
                    eventsToEvent2
                        { previousEvent = Just (MouseLeave2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                MouseOver a ->
                    eventsToEvent2
                        { previousEvent = Just (MouseOver2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                MouseEnter a ->
                    eventsToEvent2
                        { previousEvent = Just (MouseEnter2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                MouseOut a ->
                    eventsToEvent2
                        { previousEvent = Just (MouseOut2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                Focus a ->
                    eventsToEvent2
                        { previousEvent = Just (Focus2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                Blur a ->
                    eventsToEvent2
                        { previousEvent = Just (Blur2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                Wheel a ->
                    eventsToEvent2
                        { previousEvent = Just (Wheel2 clientId delay2 a)
                        , previousTime = timestamp2
                        , rest = maybeToList previousEvent ++ rest
                        }
                        events2

                File _ ->
                    eventsToEvent2 state events2


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


codegen : ParsedCode -> Settings -> List Event -> String
codegen parsedCode settings events =
    let
        existingRequests : Dict String String
        existingRequests =
            Dict.fromList parsedCode.fileRequests

        overriddenHttpRequests : List ( String, String )
        overriddenHttpRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        Http http ->
                            let
                                key =
                                    http.method ++ "_" ++ dropPrefix "http://localhost:8001/" http.url
                            in
                            case Dict.get key existingRequests of
                                Just path ->
                                    if path == http.filepath then
                                        Nothing

                                    else
                                        Just ( key, http.filepath )

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing
                )
                events

        test :
            { needsFileUploadHelper : Bool
            , needsMultipleFileUploadHelper : Bool
            , needsJsonEncodeHelper : Bool
            , code : Expression
            }
        test =
            testCode parsedCode.testName settings events overriddenHttpRequests

        testsText =
            Elm.Pretty.prettyExpression test.code
                |> Pretty.pretty 120
                |> String.replace "\n" "\n    "
                |> (\a ->
                        if parsedCode.noPriorTests then
                            a ++ "\n    "

                        else
                            "\n    , " ++ a
                   )
    in
    if settings.showAllCode then
        let
            newFileRequests : List ( String, String )
            newFileRequests =
                List.concatMap
                    (\event ->
                        case event.eventType of
                            Http http ->
                                [ ( http.method ++ "_" ++ dropPrefix "http://localhost:8001/" http.url
                                  , http.filepath
                                  )
                                ]

                            File fileUploads ->
                                List.map
                                    (\file ->
                                        ( -- This key is never used. We just need to add the file uploads here so they are included when loading all of the files needed for tests. We could have a separate list for them but it's easier to reuse this function instead of needing to parse another function out of the tests module.
                                          file.contentFilePath
                                        , file.contentFilePath
                                        )
                                    )
                                    fileUploads.files

                            _ ->
                                []
                    )
                    events

            fileRequests2 : String
            fileRequests2 =
                newFileRequests
                    ++ localRequests
                    -- existing files come last so they aren't overridden
                    ++ parsedCode.fileRequests
                    |> Dict.fromList
                    |> Dict.toList
                    |> List.map (\( first, second ) -> "( \"" ++ first ++ "\", \"" ++ second ++ "\" )")
                    |> String.join "\n    , "
                    |> (\a -> "\nfileRequests =\n    [ " ++ a ++ "\n    ]\n        ")

            localRequests : List ( String, String )
            localRequests =
                List.filterMap
                    (\event ->
                        case event.eventType of
                            HttpLocal { filepath } ->
                                Just ( "GET_" ++ filepath, "/public" ++ filepath )

                            _ ->
                                Nothing
                    )
                    events

            addHelperFunction : Bool -> String -> String -> String -> String
            addHelperFunction condition name functionText code =
                if not condition || String.contains ("\n" ++ name) code then
                    code

                else
                    code ++ "\n\n" ++ functionText
        in
        List.map
            (\codePart ->
                case codePart of
                    UserCode code ->
                        code

                    FileRequestCode ->
                        fileRequests2

                    TestEntryPoint ->
                        testsText
            )
            parsedCode.codeParts
            |> String.concat
            |> addHelperFunction test.needsFileUploadHelper "fileUploadHelper" fileUploadHelper
            |> addHelperFunction test.needsMultipleFileUploadHelper "multipleFileUploadHelper" multipleFileUploadHelper
            |> addHelperFunction test.needsJsonEncodeHelper "stringToJson" stringToJson

    else
        testsText


stringToJson : String
stringToJson =
    """
stringToJson : String -> Json.Encode.Value
stringToJson json =
    Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)"""


urlToStringNoDomain : String -> String
urlToStringNoDomain url =
    case String.split "/" url of
        "http:" :: _ :: _ :: rest ->
            "/" ++ String.join "/" rest

        "https:" :: _ :: _ :: rest ->
            "/" ++ String.join "/" rest

        _ ->
            url


{-| Copied from here <https://github.com/elmcraft/core-extra/blob/57fa91ab5cbce33c26ba6739be479c533ede063e/src/List/Extra.elm#L667>
-}
listFindIndex : (a -> Bool) -> List a -> Maybe Int
listFindIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


eventToString : Int -> Settings -> List ClientId -> List EventType2 -> List Expression
eventToString depth settings clients events =
    List.map
        (\event ->
            let
                client clientId =
                    case listFindIndex (\a -> a == clientId) clients of
                        Just index ->
                            "tab" ++ String.fromInt (index + 1)

                        Nothing ->
                            "tab"
            in
            case event of
                Connect2 clientId delay { url, sessionId, windowWidth, windowHeight } events2 ->
                    Codegen.apply
                        [ Codegen.fqFun [ "T" ] "connectFrontend"
                        , Codegen.int delay
                        , Codegen.apply
                            [ Codegen.fqFun [ "Effect", "Lamdera" ] "sessionIdFromString"
                            , Codegen.string sessionId
                            ]
                        , Codegen.string (urlToStringNoDomain url)
                        , Codegen.record
                            [ ( "width", Codegen.int windowWidth )
                            , ( "height", Codegen.int windowHeight )
                            ]
                        , Codegen.lambda
                            [ Codegen.varPattern (client clientId) ]
                            (Codegen.list
                                (eventToString (depth + 1) settings clients events2)
                            )
                        ]

                WindowResize2 clientId delay resizeEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "resizeWindow"
                        , Codegen.int delay
                        , Codegen.record
                            [ ( "width", Codegen.int resizeEvent.width )
                            , ( "height", Codegen.int resizeEvent.height )
                            ]
                        ]

                KeyDown2 clientId delay keyEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "keyDown"
                        , Codegen.int delay
                        , targetIdFunc keyEvent.targetId
                        , Codegen.string keyEvent.key
                        , Codegen.list
                            (List.filterMap
                                (\( name, bool ) ->
                                    if bool then
                                        Just (Codegen.val name)

                                    else
                                        Nothing
                                )
                                [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                , ( "Key_AltHeld", keyEvent.altKey )
                                , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                , ( "Key_MetaHeld", keyEvent.metaKey )
                                ]
                            )
                        ]

                KeyUp2 clientId delay keyEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "keyUp"
                        , Codegen.int delay
                        , targetIdFunc keyEvent.targetId
                        , Codegen.string keyEvent.key
                        , Codegen.list
                            (List.filterMap
                                (\( name, bool ) ->
                                    if bool then
                                        Just (Codegen.val name)

                                    else
                                        Nothing
                                )
                                [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                , ( "Key_AltHeld", keyEvent.altKey )
                                , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                , ( "Key_MetaHeld", keyEvent.metaKey )
                                ]
                            )
                        ]

                Click2 clientId delay mouseEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "click"
                        , Codegen.int delay
                        , targetIdFunc mouseEvent.targetId
                        ]

                ClickLink2 clientId delay mouseEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "clickLink"
                        , Codegen.int delay
                        , Codegen.string mouseEvent.path
                        ]

                Input2 clientId delay { targetId, text } ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "input"
                        , Codegen.int delay
                        , targetIdFunc targetId
                        , Codegen.string text
                        ]

                FromJsPort2 clientId delay { port_, data } ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "portEvent"
                        , Codegen.int delay
                        , Codegen.string port_
                        , stringToJsonCode data |> Tuple.second
                        ]

                PointerDown2 clientId delay a ->
                    pointerCodegen delay settings "pointerDown" (client clientId) a

                PointerUp2 clientId delay a ->
                    pointerCodegen delay settings "pointerUp" (client clientId) a

                PointerMove2 clientId delay a ->
                    pointerCodegen delay settings "pointerMove" (client clientId) a

                PointerLeave2 clientId delay a ->
                    pointerCodegen delay settings "pointerLeave" (client clientId) a

                PointerCancel2 clientId delay a ->
                    pointerCodegen delay settings "pointerCancel" (client clientId) a

                PointerOver2 clientId delay a ->
                    pointerCodegen delay settings "pointerOver" (client clientId) a

                PointerEnter2 clientId delay a ->
                    pointerCodegen delay settings "pointerEnter" (client clientId) a

                PointerOut2 clientId delay a ->
                    pointerCodegen delay settings "pointerOut" (client clientId) a

                TouchStart2 clientId delay a ->
                    touchCodegen delay "touchStart" (client clientId) a

                TouchCancel2 clientId delay a ->
                    touchCodegen delay "touchCancel" (client clientId) a

                TouchMove2 clientId delay a ->
                    touchCodegen delay "touchMove" (client clientId) a

                TouchEnd2 clientId delay a ->
                    touchCodegen delay "touchEnd" (client clientId) a

                CheckView2 clientId delay checkViewEvent ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "checkView"
                        , Codegen.int delay
                        , Codegen.apply
                            [ Codegen.fqFun [ "Test", "Html", "Query" ] "has"
                            , Codegen.list
                                (List.map
                                    (\text ->
                                        Codegen.apply
                                            [ Codegen.fqFun [ "Test", "Html", "Selector" ] "text"
                                            , Codegen.string text
                                            ]
                                    )
                                    checkViewEvent.selection
                                )
                            ]
                        ]

                MouseDown2 clientId delay a ->
                    mouseCodegen delay settings "mouseDown" (client clientId) a

                MouseUp2 clientId delay a ->
                    mouseCodegen delay settings "mouseUp" (client clientId) a

                MouseMove2 clientId delay a ->
                    mouseCodegen delay settings "mouseMove" (client clientId) a

                MouseLeave2 clientId delay a ->
                    mouseCodegen delay settings "mouseLeave" (client clientId) a

                MouseOver2 clientId delay a ->
                    mouseCodegen delay settings "mouseOver" (client clientId) a

                MouseEnter2 clientId delay a ->
                    mouseCodegen delay settings "mouseEnter" (client clientId) a

                MouseOut2 clientId delay a ->
                    mouseCodegen delay settings "mouseOut" (client clientId) a

                Focus2 clientId delay a ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "focus"
                        , Codegen.int delay
                        , targetIdFunc a.targetId
                        ]

                Blur2 clientId delay a ->
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "blur"
                        , Codegen.int delay
                        , targetIdFunc a.targetId
                        ]

                Wheel2 clientId delay a ->
                    let
                        modifiers : List Expression
                        modifiers =
                            List.filterMap
                                (\( name, value, default ) ->
                                    if value == Codegen.string default then
                                        Nothing

                                    else
                                        Just (Codegen.apply [ Codegen.val name, value ])
                                )
                                [ ( "DeltaX", Codegen.float a.deltaX, "0" )
                                , ( "DeltaZ", Codegen.float a.deltaZ, "0" )
                                , ( "DeltaMode"
                                  , case a.deltaMode of
                                        1 ->
                                            Codegen.string "DeltaLine"

                                        2 ->
                                            Codegen.string "DeltaPage"

                                        _ ->
                                            Codegen.string "DeltaPixel"
                                  , "DeltaPixel"
                                  )
                                ]
                    in
                    Codegen.apply
                        [ Codegen.access (Codegen.fun (client clientId)) "wheel"
                        , Codegen.int delay
                        , targetIdFunc a.mouseEvent.targetId
                        , Codegen.float a.deltaY
                        , Codegen.tuple
                            [ Codegen.float a.mouseEvent.offsetX
                            , Codegen.float a.mouseEvent.offsetY
                            ]
                        , Codegen.list modifiers
                        , mouseEventModifiers settings a.mouseEvent
                        ]
        )
        events


stringToJsonCode : String -> ( Bool, Expression )
stringToJsonCode data =
    case Json.Decode.decodeString Json.Decode.float data of
        Ok float ->
            ( False, Codegen.apply [ Codegen.fqFun [ "Json", "Encode" ] "float", Codegen.float float ] )

        Err _ ->
            case Json.Decode.decodeString Json.Decode.string data of
                Ok text ->
                    ( False
                    , Codegen.apply
                        [ Codegen.fqFun [ "Json", "Encode" ] "string", Codegen.string text ]
                    )

                Err _ ->
                    ( True, Codegen.apply [ Codegen.fun "stringToJson", Codegen.string data ] )


{-| Copied from here <https://github.com/elmcraft/core-extra/blob/57fa91ab5cbce33c26ba6739be479c533ede063e/src/List/Extra.elm#L455>
-}
listUnique : List a -> List a
listUnique list =
    uniqueHelp identity [] list []


uniqueHelp : (a -> b) -> List b -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
            if List.member computedFirst existing then
                uniqueHelp f existing rest accumulator

            else
                uniqueHelp f (computedFirst :: existing) rest (first :: accumulator)


testCode :
    String
    -> Settings
    -> List Event
    -> List ( String, String )
    ->
        { needsFileUploadHelper : Bool
        , needsMultipleFileUploadHelper : Bool
        , needsJsonEncodeHelper : Bool
        , code : Expression
        }
testCode testName settings events overriddenHttpRequests =
    let
        firstTime : Int
        firstTime =
            List.map (\a -> Time.posixToMillis a.timestamp) events
                |> List.minimum
                |> Maybe.withDefault 0

        clients : List ClientId
        clients =
            List.map .clientId events |> listUnique

        events2 : List Expression
        events2 =
            eventsToEvent2 { previousEvent = Nothing, previousTime = firstTime, rest = [] } events
                |> eventToString 0 settings clients

        singleFileEvents : List UploadedFile
        singleFileEvents =
            List.filterMap
                (\{ eventType } ->
                    case eventType of
                        File fileEvent ->
                            if fileEvent.allowMultiple then
                                Nothing

                            else
                                List.head fileEvent.files

                        _ ->
                            Nothing
                )
                events

        multiFileEvents : List (List UploadedFile)
        multiFileEvents =
            List.filterMap
                (\{ eventType } ->
                    case eventType of
                        File fileEvent ->
                            if fileEvent.allowMultiple then
                                Just fileEvent.files

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                events
    in
    { code =
        Codegen.apply
            [ Codegen.fqFun [ "T" ] "start"
            , Codegen.string testName
            , millisToPosix firstTime
            , case
                List.filterMap
                    (\( isEmpty, fieldAndExpression ) ->
                        if isEmpty then
                            Nothing

                        else
                            Just fieldAndExpression
                    )
                    [ ( List.isEmpty singleFileEvents, fileUploadHandler singleFileEvents )
                    , ( List.isEmpty multiFileEvents, multipleFileUploadHandler multiFileEvents )
                    , ( List.isEmpty overriddenHttpRequests, httpRequestsOverrideHandler overriddenHttpRequests )
                    ]
              of
                [] ->
                    Codegen.fun "config"

                updatedFields ->
                    Codegen.update "config" updatedFields
            , Codegen.list events2
            ]
    , needsFileUploadHelper = List.isEmpty singleFileEvents |> not
    , needsMultipleFileUploadHelper = List.isEmpty multiFileEvents |> not
    , needsJsonEncodeHelper =
        List.any
            (\{ eventType, isHidden } ->
                case ( eventType, isHidden ) of
                    ( FromJsPort event, False ) ->
                        stringToJsonCode event.data |> Tuple.first

                    _ ->
                        False
            )
            events
    }


millisToPosix : Int -> Expression
millisToPosix time =
    Codegen.apply [ Codegen.fqFun [ "Time" ] "millisToPosix", Codegen.int time ]


fileUploadHelper : String
fileUploadHelper =
    """
fileUploadHelper : Dict String Bytes -> List { name : String , lastModified : Int , filepath : String , mimeType : String } -> { a | data : T.Data frontendModel backendModel } -> T.FileUpload
fileUploadHelper files filesToUpload { data } =
    case List.drop (List.length data.fileUploads) filesToUpload |> List.head of
        Just a ->
            case Dict.get a.filePath files of
                Just bytes ->
                    T.uploadBytesFile a.name a.mimeType bytes (Time.millisToPosix a.lastModified)
                        |> T.UploadFile

                Nothing ->
                    T.UnhandledFileUpload

        Nothing ->
            T.UnhandledFileUpload"""
        |> String.replace "\u{000D}" ""


multipleFileUploadHelper : String
multipleFileUploadHelper =
    """
multipleFileUploadHelper : Dict String Bytes -> List (List { name : String, lastModified : Int, filepath : String, mimeType : String }) -> { a | data : T.Data frontendModel backendModel } -> T.MultipleFilesUpload
multipleFileUploadHelper files filesToUpload { data } =
    case List.drop (List.length data.fileUploads) filesToUpload |> List.head of
        Just a ->
            case List.filterMap (\\b -> Dict.get b.filepath files |> Maybe.map (Tuple.pair b)) a of
                ( file, bytes ) :: rest ->
                    T.UploadMultipleFiles
                        (T.uploadBytesFile file.name file.mimeType bytes (Time.millisToPosix file.lastModified))
                        (List.map
                            (\\( file2, bytes2 ) ->
                                T.uploadBytesFile
                                    file2.name
                                    file2.mimeType
                                    bytes2
                                    (Time.millisToPosix file2.lastModified)
                            )
                            rest
                        )

                [] ->
                    T.UnhandledMultiFileUpload

        Nothing ->
            T.UnhandledMultiFileUpload"""
        |> String.replace "\u{000D}" ""


fileUploadHandler : List UploadedFile -> ( String, Expression )
fileUploadHandler files =
    ( "handleFileUploads"
    , Codegen.apply
        (Codegen.fun "fileUploadHelper"
            :: Codegen.val "fileData"
            :: List.map
                (\fileUpload ->
                    Codegen.record
                        [ ( "name", Codegen.string fileUpload.name )
                        , ( "lastModified", Codegen.int fileUpload.lastModified )
                        , ( "filepath", Codegen.string fileUpload.contentFilePath )
                        , ( "mimeType", Codegen.string fileUpload.mimeType )
                        ]
                )
                files
        )
    )


multipleFileUploadHandler : List (List UploadedFile) -> ( String, Expression )
multipleFileUploadHandler files =
    ( "handleMultipleFilesUpload"
    , Codegen.apply
        [ Codegen.fun "multipleFileUploadHelper"
        , Codegen.val "fileData"
        , List.map
            (\files2 ->
                List.map
                    (\fileUpload ->
                        Codegen.record
                            [ ( "name", Codegen.string fileUpload.name )
                            , ( "lastModified", Codegen.int fileUpload.lastModified )
                            , ( "path", Codegen.string fileUpload.contentFilePath )
                            , ( "mimeType", Codegen.string fileUpload.mimeType )
                            ]
                    )
                    files2
                    |> Codegen.list
            )
            files
            |> Codegen.list
        ]
    )


httpRequestsOverrideHandler : List ( String, String ) -> ( String, Expression )
httpRequestsOverrideHandler httpRequests =
    ( "handleHttpRequest"
    , Codegen.apply
        [ Codegen.fun "handleHttpRequests"
        , Codegen.apply
            [ Codegen.fqFun [ "Dict" ] "fromList"
            , List.map
                (\( request, filepath ) -> Codegen.tuple [ Codegen.string request, Codegen.string filepath ])
                httpRequests
                |> Codegen.list
            ]
        , Codegen.val "fileData"
        ]
    )


touchCodegen : MillisecondWaitBefore -> String -> String -> TouchEvent -> Expression
touchCodegen delay funcName client a =
    let
        touchToString : Touch -> Expression
        touchToString touch =
            Codegen.record
                [ ( "id", Codegen.int touch.identifier )
                , ( "screenPos", Codegen.tuple [ Codegen.float touch.screenX, Codegen.float touch.screenY ] )
                , ( "clientPos", Codegen.tuple [ Codegen.float touch.clientX, Codegen.float touch.clientY ] )
                , ( "pagePos", Codegen.tuple [ Codegen.float touch.pageX, Codegen.float touch.pageY ] )
                ]
    in
    Codegen.apply
        [ Codegen.access (Codegen.fun client) funcName
        , Codegen.int delay
        , targetIdFunc a.targetId
        , Codegen.record
            [ ( "targetTouches", Codegen.list (List.map touchToString a.targetTouches) )
            , ( "changedTouches", Codegen.list (List.map touchToString a.changedTouches) )
            ]
        ]


pointerCodegen : MillisecondWaitBefore -> Settings -> String -> String -> PointerEvent -> Expression
pointerCodegen delay { includeClientPos, includePagePos, includeScreenPos } funcName client a =
    let
        options : List Expression
        options =
            List.filterMap
                (\( name, include, ( x, y ) ) ->
                    if (x == a.offsetX && y == a.offsetY) || not include then
                        Nothing

                    else
                        Codegen.apply
                            [ Codegen.fun name
                            , Codegen.tuple [ Codegen.float x, Codegen.float y ]
                            ]
                            |> Just
                )
                [ ( "ScreenXY", includeScreenPos, ( a.screenX, a.screenY ) )
                , ( "PageXY", includePagePos, ( a.pageX, a.pageY ) )
                , ( "ClientXY", includeClientPos, ( a.clientX, a.clientY ) )
                ]
                ++ List.filterMap
                    identity
                    [ pointerButton a
                    , if a.altKey then
                        Just (Codegen.val "AltHeld")

                      else
                        Nothing
                    , if a.shiftKey then
                        Just (Codegen.val "ShiftHeld")

                      else
                        Nothing
                    , if a.ctrlKey then
                        Just (Codegen.val "CtrlHeld")

                      else
                        Nothing
                    , if a.metaKey then
                        Just (Codegen.val "MetaHeld")

                      else
                        Nothing
                    , if a.pointerId == 0 then
                        Nothing

                      else
                        Just (Codegen.apply [ Codegen.val "PointerId ", Codegen.int a.pointerId ])
                    , if a.isPrimary then
                        Nothing

                      else
                        Just (Codegen.val "IsNotPrimary")
                    ]
    in
    Codegen.apply
        [ Codegen.access (Codegen.fun client) funcName
        , Codegen.int delay
        , targetIdFunc a.targetId
        , Codegen.tuple [ Codegen.float a.offsetX, Codegen.float a.offsetY ]
        , Codegen.list options
        ]


mouseCodegen : MillisecondWaitBefore -> Settings -> String -> String -> MouseEvent -> Expression
mouseCodegen delay settings funcName client a =
    Codegen.apply
        [ Codegen.access (Codegen.fun client) funcName
        , Codegen.int delay
        , targetIdFunc a.targetId
        , Codegen.tuple [ Codegen.float a.offsetX, Codegen.float a.offsetY ]
        , mouseEventModifiers settings a
        ]


mouseEventModifiers : Settings -> MouseEvent -> Expression
mouseEventModifiers { includeClientPos, includePagePos, includeScreenPos } a =
    List.filterMap
        (\( name, include, ( x, y ) ) ->
            if (x == a.offsetX && y == a.offsetY) || not include then
                Nothing

            else
                Codegen.apply
                    [ Codegen.val name
                    , Codegen.float x
                    , Codegen.float y
                    ]
                    |> Just
        )
        [ ( "ScreenXY", includeScreenPos, ( a.screenX, a.screenY ) )
        , ( "PageXY", includePagePos, ( a.pageX, a.pageY ) )
        , ( "ClientXY", includeClientPos, ( a.clientX, a.clientY ) )
        ]
        ++ List.filterMap
            identity
            [ pointerButton a
            , if a.altKey then
                Just (Codegen.val "AltHeld")

              else
                Nothing
            , if a.shiftKey then
                Just (Codegen.val "ShiftHeld")

              else
                Nothing
            , if a.ctrlKey then
                Just (Codegen.val "CtrlHeld")

              else
                Nothing
            , if a.metaKey then
                Just (Codegen.val "MetaHeld")

              else
                Nothing
            ]
        |> Codegen.list


pointerButton : { a | button : Int } -> Maybe Codegen.Expression
pointerButton a =
    case a.button of
        1 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "MainButton" ])

        2 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "MiddleButton" ])

        3 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "SecondButton" ])

        4 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "BackButton" ])

        5 ->
            Just (Codegen.apply [ Codegen.val "PointerButton", Codegen.val "ForwardButton" ])

        _ ->
            Nothing


targetIdFunc : Maybe String -> Expression
targetIdFunc id =
    let
        id2 : String
        id2 =
            case id of
                Just "" ->
                    "please-add-an-id"

                Just id3 ->
                    id3

                Nothing ->
                    "please-add-an-id"
    in
    Codegen.apply [ Codegen.fqFun [ "Dom" ] "id", Codegen.string id2 ]


type OutMsg
    = NoChange
    | Change (Maybe RecordingState)


updateLoaded : TestEditorMsg -> LoadedData -> RecordingState -> ( LoadedData, OutMsg, Cmd TestEditorMsg )
updateLoaded msg testEditor recording =
    case msg of
        MouseDownOnEvent index isHidden ->
            ( { testEditor | mouseDownOnEvent = True }
            , setEventVisibility index isHidden recording
                |> Just
                |> Change
            , Cmd.none
            )

        MouseEnterOnEvent index isHidden ->
            if testEditor.mouseDownOnEvent then
                ( testEditor, setEventVisibility index isHidden recording |> Just |> Change, Cmd.none )

            else
                ( testEditor, NoChange, Cmd.none )

        PressedSaveTest data ->
            case ( testEditor.parsedCode, testEditor.commitStatus ) of
                ( _, Committing _ ) ->
                    ( testEditor, NoChange, Cmd.none )

                ( ParseSuccess ok, _ ) ->
                    let
                        settings : Settings
                        settings =
                            testEditor.settings

                        text : String
                        text =
                            codegen
                                ok
                                { settings | showAllCode = True }
                                (Array.toList recording.history |> List.filter (\event -> not event.isHidden))
                    in
                    ( { testEditor | commitStatus = Committing text }
                    , NoChange
                    , write_file_to_js
                        (WroteToFile
                            { shouldOpen =
                                if data.shouldOpen then
                                    Just ok.testName

                                else
                                    Nothing
                            }
                        )
                        text
                    )

                _ ->
                    ( testEditor, NoChange, Cmd.none )

        ToggledIncludeScreenPos bool ->
            updateSettings (\settings -> { settings | includeScreenPos = bool }) testEditor

        ToggledIncludeClientPos bool ->
            updateSettings (\settings -> { settings | includeClientPos = bool }) testEditor

        ToggledIncludePagePos bool ->
            updateSettings (\settings -> { settings | includePagePos = bool }) testEditor

        MouseUpEvent ->
            ( { testEditor | mouseDownOnEvent = False }, NoChange, Cmd.none )

        PressedEvent ->
            ( testEditor, NoChange, Cmd.none )

        GotTestFile result ->
            ( { testEditor
                | parsedCode =
                    case result of
                        Ok content ->
                            case String.replace "\u{000D}" "" content |> parseCode of
                                Ok ok ->
                                    ParseSuccess ok

                                Err err ->
                                    ParseFailed err

                        Err (Http.BadStatus 404) ->
                            ParseSuccess newCode

                        Err _ ->
                            ParseFailed UnknownError
              }
            , NoChange
            , Cmd.none
            )

        ToggledShowAllCode bool ->
            updateSettings (\settings -> { settings | showAllCode = bool }) testEditor

        WroteToFile { shouldOpen } result ->
            case testEditor.commitStatus of
                Committing text ->
                    case result of
                        Ok () ->
                            let
                                _ =
                                    case shouldOpen of
                                        Just testName ->
                                            LD.debugS
                                                "current-test"
                                                { name = testName, stepIndex = 1, timelineIndex = 1 }

                                        Nothing ->
                                            { name = "", stepIndex = 0, timelineIndex = 0 }
                            in
                            ( { testEditor
                                | parsedCode =
                                    case parseCode text of
                                        Ok ok ->
                                            ParseSuccess ok

                                        Err _ ->
                                            testEditor.parsedCode
                              }
                            , Change Nothing
                            , case shouldOpen of
                                Just _ ->
                                    Browser.Navigation.load ("http://localhost:8000" ++ testPath)

                                Nothing ->
                                    Cmd.none
                            )

                        Err _ ->
                            ( { testEditor | commitStatus = CommitFailed }, NoChange, Cmd.none )

                _ ->
                    ( testEditor, NoChange, Cmd.none )

        PressedDeleteTest ->
            ( testEditor, Change Nothing, Cmd.none )


testPath : String
testPath =
    "/tests/RecordedTests.elm"


write_file_to_js : (Result Http.Error () -> msg) -> String -> Cmd msg
write_file_to_js msg text =
    Http.post
        { url = "/_x/write" ++ testPath
        , body = Http.stringBody "text/plain" text
        , expect = Http.expectWhatever msg
        }


loadTestsFile : (Result Http.Error String -> msg) -> Cmd msg
loadTestsFile msg =
    Http.post
        { url = "/_x/read" ++ testPath
        , body = Http.emptyBody
        , expect = Http.expectString msg
        }


{-| Copied from here <https://github.com/elmcraft/core-extra/blob/57fa91ab5cbce33c26ba6739be479c533ede063e/src/List/Extra.elm#L603C1-L614C36>
-}
listFind : (a -> Bool) -> List a -> Maybe a
listFind predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                listFind predicate rest


parseCodeHelper : String -> Int -> Int -> Result ParseError ParsedCode
parseCodeHelper code fileRequestsStart testsStart =
    let
        fromListIndices : List Int
        fromListIndices =
            String.indexes "|> Dict.fromList" code

        testsEndIndex : Maybe Int
        testsEndIndex =
            case listFind (\index -> index > testsStart) (String.indexes "\n    ]" code) of
                Just index ->
                    Just index

                Nothing ->
                    case listFind (\index -> index > testsStart) (String.indexes "\n    []" code) of
                        Just index ->
                            index + String.length "\n    [" |> Just

                        Nothing ->
                            Nothing
    in
    case
        ( listFind (\index -> index > fileRequestsStart) fromListIndices
        , testsEndIndex
        )
    of
        ( Just fileRequestsEnd, Just testsEnd ) ->
            let
                fileRequestsResult =
                    String.slice fileRequestsStart fileRequestsEnd code |> parseFileRequests

                sorted =
                    List.sortBy
                        (\( a, _, _ ) -> a)
                        [ ( fileRequestsStart, fileRequestsEnd, FileRequestCode )
                        , ( testsEnd, testsEnd, TestEntryPoint )
                        ]

                last : Int
                last =
                    case List.reverse sorted of
                        ( _, end, _ ) :: _ ->
                            end

                        [] ->
                            String.length code
            in
            case fileRequestsResult of
                Ok fileRequests ->
                    let
                        findValidTestName : Int -> String
                        findValidTestName index =
                            let
                                name : String
                                name =
                                    if index == 0 then
                                        "new test"

                                    else
                                        "new test " ++ String.fromInt index
                            in
                            if String.contains ("\"" ++ name ++ "\"") code then
                                findValidTestName (index + 1)

                            else
                                name
                    in
                    { codeParts =
                        List.foldl
                            (\( start, end, codeType ) state ->
                                { codeParts =
                                    codeType
                                        :: UserCode (String.slice state.previousIndex start code)
                                        :: state.codeParts
                                , previousIndex = end
                                }
                            )
                            { previousIndex = 0, codeParts = [] }
                            sorted
                            |> .codeParts
                            |> (\a -> UserCode (String.slice last (String.length code) code) :: a)
                            |> List.reverse
                    , fileRequests = fileRequests
                    , noPriorTests =
                        case String.slice testsStart testsEnd code |> String.split "\n    [" of
                            [ _, rest ] ->
                                String.contains "," rest |> not

                            _ ->
                                False
                    , testName = findValidTestName 0
                    }
                        |> Ok

                Err () ->
                    Err InvalidFileRequests

        ( Nothing, _ ) ->
            Err FileRequestsEndNotFound

        ( _, Nothing ) ->
            Err TestEntryPointNotFound


parseCode : String -> Result ParseError ParsedCode
parseCode code =
    case ( String.indexes "\nfileRequests =" code, String.indexes "\ntests : " code ) of
        ( [ fileRequestsStart ], [ testsStart ] ) ->
            parseCodeHelper code fileRequestsStart testsStart

        ( [ _ ], [] ) ->
            Err TestEntryPointNotFound

        ( [], [ _ ] ) ->
            Err FileRequestsNotFound

        _ ->
            Err UnknownError


newCode : ParsedCode
newCode =
    { codeParts =
        [ """module MyTests exposing (main, setup, tests)

import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Effect.Browser.Dom as Dom
import Effect.Lamdera
import Effect.Test as T exposing (FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..), PointerOptions(..))
import Frontend
import Json.Decode
import Json.Encode
import Test.Html.Query
import Test.Html.Selector as Selector
import Time
import Types exposing (ToBackend, FrontendMsg, FrontendModel, ToFrontend, BackendMsg, BackendModel)
import Url exposing (Url)


setup : T.ViewerWith (List (T.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
setup =
    T.viewerWith tests
        |> T.addBytesFiles (Dict.values fileRequests)


main : Program () (T.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (T.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    T.startViewer setup


domain : Url
domain =
    { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


{-| Please don't modify or rename this function -}
fileRequests : Dict String String"""
            |> String.replace "\u{000D}" ""
            |> UserCode
        , FileRequestCode
        , """|> Dict.fromList


handleHttpRequests : Dict String String -> Dict String Bytes -> { currentRequest : HttpRequest, data : T.Data FrontendModel BackendModel } -> HttpResponse
handleHttpRequests overrides fileData { currentRequest } =
    let
        key : String
        key =
            currentRequest.method ++ "_" ++ currentRequest.url

        getData : String -> HttpResponse
        getData path =
            case Dict.get path fileData of
                Just data ->
                    BytesHttpResponse { url = currentRequest.url, statusCode = 200, statusText = "OK", headers = Dict.empty } data

                Nothing ->
                    UnhandledHttpRequest
    in
    case ( Dict.get key overrides, Dict.get key fileRequests ) of
        ( Just path, _ ) ->
            getData path

        ( Nothing, Just path ) ->
            getData path

        _ ->
            UnhandledHttpRequest


{-| You can change parts of this function represented with `...`.
The rest needs to remain unchanged in order for the test generator to be able to add new tests.

    tests : ... -> List (T.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
    tests ... =
        let
            config = ...

            ...
        in
        [ ...
        ]
-}
tests : Dict String Bytes -> List (T.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests fileData =
    let
        config =
            T.Config
                Frontend.app_
                Backend.app_
                (handleHttpRequests Dict.empty fileData)
                (\\_ -> Nothing)
                (\\_ -> UnhandledFileUpload)
                (\\_ -> UnhandledMultiFileUpload)
                domain
    in
    ["""
            |> String.replace "\u{000D}" ""
            |> UserCode
        , TestEntryPoint
        , UserCode "\n    ]"
        ]
    , fileRequests = []
    , noPriorTests = True
    , testName = "new test"
    }


parseFileRequests : String -> Result () (List ( String, String ))
parseFileRequests code =
    case Elm.Parser.parseToFile ("module A exposing (..)\n" ++ code) of
        Ok ast ->
            case ast.declarations of
                [ Node _ (FunctionDeclaration func) ] ->
                    case Node.value func.declaration |> .expression of
                        Node _ (ListExpr requests) ->
                            List.filterMap
                                (\(Node _ request) ->
                                    case request of
                                        TupledExpression [ Node _ (Literal a), Node _ (Literal b) ] ->
                                            Just ( a, b )

                                        _ ->
                                            Nothing
                                )
                                requests
                                |> Ok

                        _ ->
                            Err ()

                _ ->
                    Err ()

        Err _ ->
            Err ()


setEventVisibility : Int -> Bool -> RecordingState -> RecordingState
setEventVisibility index isHidden recording =
    case Array.get index recording.history of
        Just event ->
            if event.isHidden == isHidden then
                recording

            else
                { recording | history = Array.set index { event | isHidden = isHidden } recording.history }

        Nothing ->
            recording


updateSettings : (Settings -> Settings) -> LoadedData -> ( LoadedData, OutMsg, Cmd frontendMsg )
updateSettings updateFunc testEditor =
    let
        settings2 =
            updateFunc testEditor.settings
    in
    ( { testEditor | settings = settings2 }, NoChange, Cmd.none )


addEvent : Event -> RecordingState -> RecordingState
addEvent event recording =
    let
        event2 : Event
        event2 =
            { event
                | isHidden =
                    case event.eventType of
                        KeyUp keyEvent ->
                            shouldHideKeyEvent keyEvent

                        KeyDown keyEvent ->
                            shouldHideKeyEvent keyEvent

                        _ ->
                            False
            }

        history2 : Array Event
        history2 =
            case Array.get (Array.length recording.history - 1) recording.history of
                Just last ->
                    if Time.posixToMillis event2.timestamp - Time.posixToMillis last.timestamp < 0 then
                        Array.push event2 recording.history
                            |> Array.toList
                            |> List.sortBy (\a -> Time.posixToMillis a.timestamp)
                            |> Array.fromList

                    else
                        Array.push event2 recording.history

                Nothing ->
                    Array.push event2 recording.history
    in
    { recording
        | history =
            Array.foldr
                (\item state ->
                    case state.latestEvent of
                        Just { timestamp, previous } ->
                            if Time.posixToMillis timestamp - Time.posixToMillis item.timestamp > 2000 then
                                state

                            else
                                { latestEvent =
                                    Just
                                        { timestamp = timestamp
                                        , previous =
                                            if item.isHidden then
                                                previous

                                            else
                                                item
                                        }
                                , array =
                                    case ( previous.eventType, item.eventType, previous.clientId == item.clientId ) of
                                        ( Input _, Input _, True ) ->
                                            Array.set state.index { item | isHidden = True } state.array

                                        _ ->
                                            state.array
                                , index = state.index - 1
                                }

                        Nothing ->
                            { state | latestEvent = Just { timestamp = item.timestamp, previous = item }, index = state.index - 1 }
                )
                { index = Array.length history2 - 1, latestEvent = Nothing, array = history2 }
                history2
                |> .array
    }


shouldHideKeyEvent : KeyEvent -> Bool
shouldHideKeyEvent keyEvent =
    (String.length keyEvent.key == 1 || keyEvent.key == "Shift" || keyEvent.key == "Backspace")
        && not keyEvent.altKey
        && not keyEvent.ctrlKey
        && not keyEvent.metaKey
