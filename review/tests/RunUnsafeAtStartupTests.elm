module RunUnsafeAtStartupTests exposing (all)

import Review.Test
import RunUnsafeAtStartup
import Test exposing (Test)


all : Test
all =
    Test.describe "UnsafeConstantsOnly"
        [ Test.test "Handle top level with arguments" <|
            \() ->
                """module Pages.SignOff exposing (..)

import Unsafe

a b = Unsafe.emailAddress ""
          """
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run RunUnsafeAtStartup.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Unsafe.emailAddress"
                            }
                        ]
        , Test.test "Handle lambdas" <|
            \() ->
                """module Pages.SignOff exposing (..)

import Unsafe

a = \\b -> Unsafe.emailAddress ""
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run RunUnsafeAtStartup.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Unsafe.emailAddress"
                            }
                        ]
        , Test.test "Handle let declarations" <|
            \() ->
                """module Pages.SignOff exposing (..)

import Unsafe

a =
    let
        b c = Unsafe.emailAddress ""
    in
    b
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run RunUnsafeAtStartup.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Unsafe.emailAddress"
                            }
                        ]
        , Test.test "Handle let declarations 2" <|
            \() ->
                """module Pages.SignOff exposing (..)

import Unsafe

a =
    let
        b c = ""
        d = Unsafe.emailAddress ""
    in
    b
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run RunUnsafeAtStartup.rule
                    |> Review.Test.expectNoErrors
        ]


message : String
message =
    "Only use Unsafe functions in places where they will be evaluated at app startup (aka constant functions and not inside lambdas)"


details : List String
details =
    [ "If you use an Unsafe function in a function that isn't evaluated at program start, you won't know if it will crash until potentially much later (if you're especially unlucky, that could be in production)." ]
