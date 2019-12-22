module AssemblerV2 exposing (RegexParseResult, assemble, getValue, regex, regexParse)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Regex



--Based on https://github.com/Schweigi/assembler-simulator/blob/master/src/assembler/asm.js


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



--// REGISTER, NUMBER or LABEL


parseRegOrNumber : String -> Reg -> Reg -> ( Reg, Byte )
parseRegOrNumber input rega regb =
    ( Address, mkByte 0 )


type Reg
    = Address
    | Register
    | Numbers
    | Number
    | RegisterAddress


getValue input =
    case String.split "" input of
        "[" :: rest ->
            let
                array =
                    Array.fromList rest

                length =
                    Array.length array

                string =
                    array
                        |> Array.slice 0 (length - 1)
                        |> Array.toList
                        |> String.concat
            in
            parseRegOrNumber string RegisterAddress Address

        _ ->
            parseRegOrNumber input Register Number


parseMov op1 op2 =
    Ok [ mkByte 7 ]


toBytes regexParseResult =
    case regexParseResult.instruction of
        Just "HLT" ->
            Ok [ mkByte 0 ]

        Just "MOV" ->
            parseMov regexParseResult.operand1 regexParseResult.operand1

        Just i ->
            Err <| String.concat [ "Error: Unrecognized opcdoe: ", i ]

        Nothing ->
            Err <| String.concat [ "Error: opcode expected" ]


assemble string =
    string
        |> String.lines
        |> List.map regexParse
        |> List.map toBytes
