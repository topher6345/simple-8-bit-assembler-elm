module AssemblerV2 exposing (RegexParseResult, assemble, getValue, parseLabel, parseRegOrNumber, parseRegister, regex, regexParse, toBytes)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Regex



-- Based on https://github.com/Schweigi/assembler-simulator/blob/master/src/assembler/asm.js
-- Use https://www.debuggex.com/
-- Matches: "label: INSTRUCTION (["')OPERAND1(]"'), (["')OPERAND2(]"')
-- GROUPS:      1       2               3                    7


type Value
    = ValueString String
    | ValueByte Byte


type alias RegexParseResult =
    { label : String
    , instruction : String
    , operand1 : String
    , operand2 : String
    }


mkRegex string =
    string
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


regex =
    mkRegex
        "^[\\t ]*(?:([.A-Za-z]\\w*)[:])?(?:[\\t ]*([A-Za-z]{2,4})(?:[\\t ]+(\\[(\\w+((\\+|-)\\d+)?)\\]|\\\".+?\\\"|\\'.+?\\'|[.A-Za-z0-9]\\w*)(?:[\\t ]*[,][\\t ]*(\\[(\\w+((\\+|-)\\d+)?)\\]|\\\".+?\\\"|\\'.+?\\'|[.A-Za-z0-9]\\w*))?)?)?"


regexNum =
    mkRegex "^[-+]?[0-9]+$"


regexLabel =
    mkRegex "^[.A-Za-z]\\w*$"


regexFind string =
    case Regex.find regex string of
        { submatches } :: [] ->
            submatches

        _ ->
            []


regexParse string =
    let
        fetch i =
            regexFind string
                |> Array.fromList
                |> Array.get i
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


lookupInstruction instr =
    case instr of
        "DB" ->
            Ok []

        "HLT" ->
            Err "Not Implemented"

        "MOV" ->
            Err "Not Implemented"

        "ADD" ->
            Err "Not Implemented"

        "SUB" ->
            Err "Not Implemented"

        "INC" ->
            Err "Not Implemented"

        "DEC" ->
            Err "Not Implemented"

        "CMP" ->
            Err "Not Implemented"

        "JMP" ->
            Err "Not Implemented"

        "JC" ->
            Err "Not Implemented"

        "JB" ->
            Err "Not Implemented"

        "JNAE" ->
            Err "Not Implemented"

        "JNC" ->
            Err "Not Implemented"

        "JNB" ->
            Err "Not Implemented"

        "JAE" ->
            Err "Not Implemented"

        "JZ" ->
            Err "Not Implemented"

        "JE" ->
            Err "Not Implemented"

        "JNZ" ->
            Err "Not Implemented"

        "JNE" ->
            Err "Not Implemented"

        "JA" ->
            Err "Not Implemented"

        "JNBE" ->
            Err "Not Implemented"

        "JNA" ->
            Err "Not Implemented"

        "JBE" ->
            Err "Not Implemented"

        "PUSH" ->
            Err "Not Implemented"

        "POP" ->
            Err "Not Implemented"

        "CALL" ->
            Err "Not Implemented"

        "RET" ->
            Err "Not Implemented"

        "MUL" ->
            Err "Not Implemented"

        "DIV" ->
            Err "Not Implemented"

        "AND" ->
            Err "Not Implemented"

        "OR" ->
            Err "Not Implemented"

        "XOR" ->
            Err "Not Implemented"

        "NOT" ->
            Err "Not Implemented"

        "SHL" ->
            Err "Not Implemented"

        "SAL" ->
            Err "Not Implemented"

        "SHR" ->
            Err "Not Implemented"

        "SAR" ->
            Err "Not Implemented"

        _ ->
            Err "Not Implemented"


parse input =
    let
        code =
            []

        mapping =
            []

        labels =
            []
    in
    ( code, mapping, labels )
