module Main exposing (Model, Msg(..), app, main)

-- Copied from https://elm-lang.org/examples/quotes with tweaks to make it use Effect modules
--
-- https://elm-lang.org/examples/quotes
--
-- Press a button to send a GET request for random quotes.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--

import Browser
import Effect.Browser
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Http as Http
import Effect.Subscription as Subscription exposing (Subscription)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, field, int, map4, string)



-- MAIN


main =
    Effect.Browser.element app


app =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
    = Failure
    | Loading
    | Success Quote


type alias Quote =
    { quote : String
    , source : String
    , author : String
    , year : Int
    }


init : () -> ( Model, Command FrontendOnly toMsg Msg )
init _ =
    ( Loading, getRandomQuote )



-- UPDATE


type Msg
    = MorePlease
    | GotQuote (Result Http.Error Quote)


update : Msg -> Model -> ( Model, Command FrontendOnly toMsg Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomQuote )

        GotQuote result ->
            case result of
                Ok quote ->
                    ( Success quote, Command.none )

                Err _ ->
                    ( Failure, Command.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Subscription FrontendOnly Msg
subscriptions model =
    Subscription.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Quotes" ]
        , viewQuote model
        ]


viewQuote : Model -> Html Msg
viewQuote model =
    case model of
        Failure ->
            div []
                [ text "I could not load a random quote for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success quote ->
            div [ style "max-width" "600px" ]
                [ button
                    [ onClick MorePlease
                    , style "display" "block"
                    , Html.Attributes.id "more-please"
                    ]
                    [ text "More Please!" ]
                , blockquote [] [ text quote.quote ]
                , p [ style "text-align" "right" ]
                    [ text "— "
                    , cite [] [ text quote.source ]
                    , text (" by " ++ quote.author ++ " (" ++ String.fromInt quote.year ++ ")")
                    ]
                ]



-- HTTP


getRandomQuote : Command FrontendOnly toMsg Msg
getRandomQuote =
    Http.get
        { url = "https://elm-lang.org/api/random-quotes"
        , expect = Http.expectJson GotQuote quoteDecoder
        }


quoteDecoder : Decoder Quote
quoteDecoder =
    map4 Quote
        (field "quote" string)
        (field "source" string)
        (field "author" string)
        (field "year" int)
