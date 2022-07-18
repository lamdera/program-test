module TestGenerator exposing (..)

import Browser
import Codec exposing (Codec)
import Effect.Browser.Dom as Dom
import Html exposing (Html)
import Json.Decode
import Url exposing (Url)


type Event
    = ClickedButton Dom.HtmlId
    | ClickedLink String
    | ClickedInput Dom.HtmlId
    | TypedInput Dom.HtmlId String


type alias Size =
    { width : Int, height : Int }


sizeCodec : Codec Size
sizeCodec =
    Codec.object Size
        |> Codec.field "width" .width Codec.int
        |> Codec.field "height" .height Codec.int
        |> Codec.buildObject


type alias SessionLog =
    { initialWindowSize : Size
    , initialUrl : Url
    , events : List Event
    }


sessionLogCodec : Codec SessionLog
sessionLogCodec =
    Codec.object SessionLog
        |> Codec.field "initialWindowSize" .initialWindowSize sizeCodec
        |> Codec.field "initialUrl" .initialUrl urlCodec
        |> Codec.field "events" .events (Codec.list eventCodec)
        |> Codec.buildObject


urlCodec : Codec Url
urlCodec =
    Codec.andThen
        (\text ->
            case Url.fromString text of
                Just url ->
                    Codec.succeed url

                Nothing ->
                    Codec.fail "Invalid url"
        )
        Url.toString
        Codec.string


defaultText =
    """
    {
      "initialWindowSize": {
        "width": 1279,
        "height": 1315
      },
      "initialUrl": "http://localhost:8000/",
      "events": [
        {
          "args": [
            "homePage_addressInput"
          ],
          "tag": "clickedInput"
        },
        {
          "args": [
            "homePage_submitButton"
          ],
          "tag": "clickedButton"
        },
        {
          "args": [
            "homePage_addressInput"
          ],
          "tag": "clickedInput"
        },
        {
          "tag": "typedInput",
          "args": [
            "homePage_addressInput",
            "a"
          ]
        },
        {
          "tag": "typedInput",
          "args": [
            "homePage_addressInput",
            "s"
          ]
        },
        {
          "tag": "typedInput",
          "args": [
            "homePage_addressInput",
            "d"
          ]
        },
        {
          "tag": "typedInput",
          "args": [
            "homePage_addressInput",
            "f"
          ]
        },
        {
          "args": [
            "/"
          ],
          "tag": "clickedLink"
        },
        {
          "args": [
            "/frequently-asked-questions"
          ],
          "tag": "clickedLink"
        }
      ]
    }"""


main =
    Browser.sandbox
        { init = ""
        , update = update
        , view = view
        }


update text _ =
    text


view : String -> Html msg
view text =
    case Codec.decodeString sessionLogCodec defaultText of
        Ok sessionLog ->
            { sessionLog | events = flattenEvents sessionLog.events }
                |> generateTest
                |> Html.text
                |> List.singleton
                |> Html.pre []

        Err error ->
            Html.text (Json.Decode.errorToString error)


generateTest : SessionLog -> String
generateTest sessionLog =
    let
        { width, height } =
            sessionLog.initialWindowSize

        instructions =
            List.filterMap
                (\event ->
                    case event of
                        ClickedButton id ->
                            """                |> shortPause
                |> client.clickButton \"""" ++ Dom.idToString id ++ "\"" |> Just

                        ClickedLink href ->
                            """                |> shortPause
                |> client.clickLink { href = \"""" ++ href ++ "\" }" |> Just

                        ClickedInput id ->
                            Nothing

                        TypedInput id string ->
                            """                |> shortPause
                |> client.inputText \"""" ++ Dom.idToString id ++ "\" \"" ++ string ++ "\"" |> Just
                )
                sessionLog.events
                |> String.join "\n"
    in
    """Effect.Test.start httpRequestHandler "Test name"
    |> Effect.Test.connectFrontend
        "sessionId"
        \"""" ++ Url.toString sessionLog.initialUrl ++ """"
        { width = """ ++ String.fromInt width ++ ", height = " ++ String.fromInt height ++ """ }
        (\\( instructions, client ) ->
            instructions
""" ++ instructions ++ """
        )"""


flattenEvents : List Event -> List Event
flattenEvents events =
    List.foldl
        (\nextEvent state ->
            case ( nextEvent, state.events ) of
                ( TypedInput _ "Shift", _ ) ->
                    { events = state.events }

                ( TypedInput aId aKey, (TypedInput bId bKey) :: rest ) ->
                    if aId == bId then
                        { events = TypedInput bId (bKey ++ aKey) :: rest }

                    else
                        { events = nextEvent :: state.events }

                _ ->
                    { events = nextEvent :: state.events }
        )
        { events = [] }
        events
        |> .events
        |> List.reverse


eventCodec : Codec Event
eventCodec =
    Codec.custom
        (\a b c d value ->
            case value of
                ClickedButton data0 ->
                    a data0

                ClickedLink data0 ->
                    b data0

                ClickedInput data0 ->
                    c data0

                TypedInput data0 data1 ->
                    d data0 data1
        )
        |> Codec.variant1 "clickedButton" ClickedButton htmlIdCodec
        |> Codec.variant1 "clickedLink" ClickedLink Codec.string
        |> Codec.variant1 "clickedInput" ClickedInput htmlIdCodec
        |> Codec.variant2 "typedInput" TypedInput htmlIdCodec Codec.string
        |> Codec.buildCustom


htmlIdCodec : Codec Dom.HtmlId
htmlIdCodec =
    Codec.map Dom.id Dom.idToString Codec.string
