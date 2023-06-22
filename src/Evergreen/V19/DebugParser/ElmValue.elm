module Evergreen.V19.DebugParser.ElmValue exposing (..)


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


type SequenceType
    = SeqSet
    | SeqList
    | SeqArray
    | SeqTuple


type ExpandableValue
    = ElmSequence SequenceType (List ElmValue)
    | ElmType String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))
    | ElmDict (List ( ElmValue, ElmValue ))


type ElmValue
    = Plain PlainValue
    | Expandable ExpandableValue
