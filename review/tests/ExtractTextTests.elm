module ExtractTextTests exposing (all)

import ExtractText exposing (rule)
import Review.Test
import Test exposing (Test)


all : Test
all =
    Test.describe "ExtractText"
        [ Test.test "First time" <|
            \() ->
                """module Pages.SignOff exposing (..)

import Ui

a = "abcd"
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run (rule [ [ "Pages", "SignOff" ] ])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This module has hardcoded string literals that should be replaced with record fields."
                            , details =
                                [ "We want to avoid hardcoding string literals here for the following possible reasons:"
                                , "1. Reduce bundle size"
                                , "2. Multiple written languages support"
                                , "3. Text is sensitive and shouldn't be included in the frontend bundle"
                                ]
                            , under = "module"
                            }
                            |> Review.Test.whenFixed
                                ("""module Pages.SignOff exposing (..)

import Ui
english : Texts
english =
    { abcd = "abcd"
    }

{-| Please don't rename. ExtractText depends on this specific name. -}
type alias Texts =
    { abcd : String    }


a t  = t.abcd
"""
                                    |> String.replace "\u{000D}" ""
                                )
                        ]
        , Test.test "Second time" <|
            \() ->
                """module Pages.SignOff exposing (..)

import Ui


english : Texts
english =
    { abcd = "abcd"
    }


{-| Please don't rename. ExtractText depends on this specific name. -}
type alias Texts =
    { abcd : String }


a t  = Ui.row [] [ Ui.text t.abcd, Ui.text "1234" ]
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run (rule [ [ "Pages", "SignOff" ] ])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This module has hardcoded string literals that should be replaced with record fields."
                            , details =
                                [ "We want to avoid hardcoding string literals here for the following possible reasons:"
                                , "1. Reduce bundle size"
                                , "2. Multiple written languages support"
                                , "3. Text is sensitive and shouldn't be included in the frontend bundle"
                                ]
                            , under = "module"
                            }
                            |> Review.Test.whenFixed
                                ("""module Pages.SignOff exposing (..)

import Ui


english : Texts
english =
    { abcd = "abcd"
    , num_1234 = "1234"
    }


{-| Please don't rename. ExtractText depends on this specific name. -}
type alias Texts =
    { abcd : String
    , num_1234 : String }


a t  = Ui.row [] [ Ui.text t.abcd, Ui.text t.num_1234 ]
"""
                                    |> String.replace "\u{000D}" ""
                                )
                        ]
        ]
