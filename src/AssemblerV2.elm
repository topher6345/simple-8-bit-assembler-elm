module AssemblerV2 exposing (RegexParseResult, assemble, getValue, parseLabel, parseRegister, regex, regexParse)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Regex



--Based on https://github.com/Schweigi/assembler-simulator/blob/master/src/assembler/asm.js


type Value
    = ValueString String
    | ValueByte Byte


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


parseRegister input =
    case String.toUpper input of
        "A" ->
            Just (mkByte 0)

        "B" ->
            Just (mkByte 1)

        "C" ->
            Just (mkByte 2)

        "D" ->
            Just (mkByte 3)

        _ ->
            Nothing



--// REGISTER, NUMBER or LABEL


parseNumber input =
    Just (mkByte 0)


parseOffsetAddressing input =
    Just (mkByte 0)



--MATCHES: "(.L)abel"


labelRegex =
    Regex.fromString "^[.A-Za-z]\\w*$"
        |> Maybe.withDefault Regex.never


parseLabel input =
    input
        |> Regex.find labelRegex
        |> List.map .match
        |> List.head


parseRegOrNumber : String -> Reg -> Reg -> Result String ( Reg, Value )
parseRegOrNumber input regType numberType =
    case parseRegister input of
        Just byte ->
            Ok ( Register, ValueByte byte )

        Nothing ->
            case parseLabel input of
                Just string ->
                    Ok ( Number, ValueString string )

                Nothing ->
                    case ( regType, parseOffsetAddressing input ) of
                        ( RegisterAddress, Just byte ) ->
                            Ok ( regType, ValueByte byte )

                        _ ->
                            case parseNumber input of
                                Just byte ->
                                    Ok ( numberType, ValueByte byte )

                                Nothing ->
                                    Err "Not a number"


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
