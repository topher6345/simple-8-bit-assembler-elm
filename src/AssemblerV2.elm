module AssemblerV2 exposing (RegexParseResult, assemble, regex, regexParse)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Regex


type alias RegexParseResult =
    { label : String
    , instruction : String
    , operand1 : String
    , operand2 : String
    }


regex =
    Maybe.withDefault Regex.never <|
        Regex.fromString
            "^[\\t ]*(?:([.A-Za-z]\\w*)[:])?(?:[\\t ]*([A-Za-z]{2,4})(?:[\\t ]+(\\[(\\w+((\\+|-)\\d+)?)\\]|\\\".+?\\\"|\\'.+?\\'|[.A-Za-z0-9]\\w*)(?:[\\t ]*[,][\\t ]*(\\[(\\w+((\\+|-)\\d+)?)\\]|\\\".+?\\\"|\\'.+?\\'|[.A-Za-z0-9]\\w*))?)?)?"


regexParse string =
    let
        result =
            Array.fromList <|
                case Regex.find regex string of
                    x :: [] ->
                        x.submatches

                    _ ->
                        []

        fetch i =
            Array.get i result
                |> Maybe.withDefault Nothing
    in
    { label = fetch 0
    , instruction = fetch 1
    , operand1 = fetch 2
    , operand2 = fetch 6
    }


toBytes regexParseResult =
    case regexParseResult.instruction of
        Just "HLT" ->
            Ok [ mkByte 0 ]

        Just "MOV" ->
            Ok [ mkByte 7 ]

        Just i ->
            Err <| String.concat [ "Error: Unrecognized opcdoe: ", i ]

        Nothing ->
            Err <| String.concat [ "Error: opcode expected" ]


assemble string =
    string
        |> String.lines
        |> List.map regexParse
        |> List.map toBytes
