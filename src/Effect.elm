module Effect exposing
    ( Effect
    , PortToJs
    , batch
    , fileToUrl
    , map
    , navigationLoad
    , navigationPushUrl
    , navigationReplaceUrl
    , none
    , selectFile
    , sendToBackend
    , sendToFrontend
    , sendToJs
    )

import Json.Encode
import MockFile
import SimulatedTask exposing (BackendOnly, FrontendOnly, SimulatedTask)
import TestId exposing (ClientId)
import TestInternal exposing (Effect(..), NavigationKey, Subscription(..))


type alias Effect restriction toMsg msg =
    TestInternal.Effect restriction toMsg msg


batch : List (Effect restriction toMsg msg) -> Effect restriction toMsg msg
batch =
    Batch


none : Effect restriction toMsg msg
none =
    None


sendToBackend : toMsg -> Effect FrontendOnly toMsg msg
sendToBackend =
    SendToBackend


navigationPushUrl : NavigationKey -> String -> Effect restriction toMsg msg
navigationPushUrl =
    NavigationPushUrl


navigationReplaceUrl : NavigationKey -> String -> Effect restriction toMsg msg
navigationReplaceUrl =
    NavigationReplaceUrl


navigationLoad : String -> Effect restriction toMsg msg
navigationLoad =
    NavigationLoad


selectFile : List String -> (MockFile.File -> msg) -> Effect FrontendOnly toMsg msg
selectFile =
    SelectFile


fileToUrl : (String -> msg) -> MockFile.File -> Effect FrontendOnly toMsg msg
fileToUrl =
    FileToUrl


sendToJs : String -> (Json.Encode.Value -> Cmd msg) -> Json.Encode.Value -> Effect FrontendOnly toMsg msg
sendToJs =
    Port


sendToFrontend : ClientId -> toMsg -> Effect BackendOnly toMsg msg
sendToFrontend =
    SendToFrontend


type alias PortToJs =
    { portName : String, value : Json.Encode.Value }


map :
    (toBackendA -> toBackendB)
    -> (frontendMsgA -> frontendMsgB)
    -> Effect restriction toBackendA frontendMsgA
    -> Effect restriction toBackendB frontendMsgB
map mapToMsg mapMsg frontendEffect =
    case frontendEffect of
        Batch frontendEffects ->
            List.map (map mapToMsg mapMsg) frontendEffects |> Batch

        None ->
            None

        SendToBackend toMsg ->
            mapToMsg toMsg |> SendToBackend

        NavigationPushUrl navigationKey url ->
            NavigationPushUrl navigationKey url

        NavigationReplaceUrl navigationKey url ->
            NavigationReplaceUrl navigationKey url

        NavigationLoad url ->
            NavigationLoad url

        SelectFile mimeTypes msg ->
            SelectFile mimeTypes (msg >> mapMsg)

        FileToUrl msg file ->
            FileToUrl (msg >> mapMsg) file

        Task simulatedTask ->
            SimulatedTask.map mapMsg simulatedTask
                |> SimulatedTask.mapError mapMsg
                |> Task

        Port portName function value ->
            Port portName (function >> Cmd.map mapMsg) value

        SendToFrontend clientId toMsg ->
            SendToFrontend clientId (mapToMsg toMsg)
