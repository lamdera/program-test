module Frontend exposing (Model, app, app_)

import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription
import Html exposing (Html, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import Lamdera
import Types exposing (..)


type alias Model =
    FrontendModel


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        app_


app_ =
    { init = \_ _ -> init
    , update = update
    , updateFromBackend = updateFromBackend
    , view =
        \model ->
            { title = "v1"
            , body = [ view model ]
            }
    , subscriptions = \_ -> Subscription.none
    , onUrlChange = \_ -> FNoop
    , onUrlRequest = \_ -> FNoop
    }


init : ( Model, Command FrontendOnly ToBackend FrontendMsg )
init =
    ( { counter = 0, clientId = Nothing }, Command.none )


update : FrontendMsg -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Effect.Lamdera.sendToBackend CounterIncremented )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Effect.Lamdera.sendToBackend CounterDecremented )

        FNoop ->
            ( model, Command.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue clientId ->
            ( { model | counter = newValue, clientId = Just clientId }, Command.none )


view : Model -> Html FrontendMsg
view model =
    Html.div [ style "padding" "30px" ]
        [ Html.button [ onClick Increment, id "plusOne" ] [ text "+" ]
        , Html.text (String.fromInt model.counter)
        , Html.button [ onClick Decrement, id "minusOne" ] [ text "-" ]
        , Html.div [] [ Html.text "Click me then refresh me!" ]
        ]
