module Frontend exposing (app)

import Browser
import Effect.Command
import Effect.Lamdera
import Effect.Subscription
import Html
import Html.Attributes
import Lamdera
import Types


type alias Model =
    Types.FrontendModel


type alias Msg =
    Types.FrontendMsg


app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        { init = \_ _ -> ( Nothing, Effect.Command.none )
        , update = \_ model -> ( model, Effect.Command.none )
        , updateFromBackend = \(Types.ConnectionCount count) _ -> ( Just count, Effect.Command.none )
        , view = view
        , subscriptions = \_ -> Effect.Subscription.none
        , onUrlChange = \_ -> ()
        , onUrlRequest = \_ -> ()
        }


view : Model -> Browser.Document Msg
view model =
    { title = "On Connection Example"
    , body =
        [ Html.div
            [ Html.Attributes.style "margin" "1em"
            , Html.Attributes.style "font-family" "monospace"
            ]
            [ Html.text
                (case model of
                    Nothing ->
                        "No message from the backend yet"

                    Just 1 ->
                        "The backend currently has 1 active connection"

                    Just counter ->
                        "The backend currently has " ++ String.fromInt counter ++ " active connections"
                )
            ]
        ]
    }
