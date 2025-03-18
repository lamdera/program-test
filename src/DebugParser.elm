module DebugParser exposing
    ( ElmValue(..)
    , ExpandableValue(..)
    , PlainValue(..)
    , SequenceType(..)
    , valueToElmValue
    )

import Elm.Kernel.DebugParser
import SeqDict exposing (SeqDict)


{-| Alias to represent parsed log.

Tag is part of the log message before the first colon.

-}
type alias ParsedLog =
    { tag : String
    , value : ElmValue
    }


{-| Elm values that are parsed can be either plain values like Strings and Bools, or they can be expandable values like Records, Dicts etc.

Expandable values has bool as their first parameter, which is used to indicate whether they are expanded or collapsed. This is used in UI
to show either the short version of the expandable value or the full, expanded one. It is part of the parsed type because performance of adding
it later on large models is really costly. This might change in the upcoming versions of this parser.

-}
type ElmValue
    = Plain PlainValue
    | Expandable ExpandableValue


{-| Plain values
-}
type PlainValue
    = ElmString String
    | ElmChar Char
    | ElmNumber Float
    | ElmBool Bool
    | ElmFunction
    | ElmInternals
    | ElmUnit
    | ElmFile String
    | ElmBytes Int


{-| Expandable values
-}
type ExpandableValue
    = ElmSequence SequenceType (List ElmValue)
    | ElmType String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))
    | ElmDict (SeqDict ElmValue ElmValue)


{-| Sequence type

All List-like structures are the same, so we parsed them into `ElmSequence` type. However we would like to keep the information about which
type the sequence was.

-}
type SequenceType
    = SeqSet
    | SeqList
    | SeqArray
    | SeqTuple


valueToElmValue : a -> ElmValue
valueToElmValue a =
    Elm.Kernel.DebugParser.toString a
