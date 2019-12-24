module Assembler exposing (Argument(..), OpcodeAirty2, Opcodes(..), Ram, addressConstant, addressRegister, argumentToBytes, arguments, assembleCode, assembleLine, charChomper, charConstant, constant, opcode, opcodeAirty0, opcodeAirty1, opcodeToBytes, register, toBytes)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, chompIf, chompWhile, commit, end, float, getChompedString, keyword, loop, map, oneOf, problem, run, spaces, succeed, symbol, token)
import Regex



-- General purpose (GP) register: A, B, C, D
-- Stack pointer register: SP
-- Address using a GP register: [A]
-- Address using a GP register and offset: [D-3]
-- Address using SP register and offset: [SP+2]
-- Address using a constant: [100]
-- Address using a label: label
-- Constant: Any number between 0..255 (8bit unsigned)
-- Offset for indirect addressing: Integer between -16..+15 (sign is mandatory)


type alias OpcodeAirty2 =
    { x : String
    , y : Argument
    , z : Argument
    }


type alias OpcodeAirty1 =
    { x : String
    , y : Argument
    }


type alias OpcodeAirty0 =
    { x : String }


type Argument
    = Constant String
    | AddressRegister String
    | AddressConstant String
    | CharConstant String
    | Register String


type Opcodes
    = A0 OpcodeAirty0
    | A1 OpcodeAirty1
    | A2 OpcodeAirty2


registerToByte string =
    case string of
        "A" ->
            mkByte 0

        "B" ->
            mkByte 1

        "C" ->
            mkByte 2

        "D" ->
            mkByte 3

        _ ->
            mkByte 127


argumentToBytes argument =
    case argument of
        Constant string ->
            string
                |> String.toInt
                |> Maybe.withDefault 0
                |> mkByte

        AddressRegister string ->
            registerToByte string

        AddressConstant string ->
            string
                |> String.toInt
                |> Maybe.withDefault 0
                |> mkByte

        CharConstant string ->
            string
                |> String.toList
                |> List.head
                |> Maybe.withDefault '0'
                |> Char.toCode
                |> mkByte

        Register string ->
            registerToByte string


charChomper : (Char -> Bool) -> Parser String
charChomper predicate =
    Parser.chompIf predicate
        |> Parser.getChompedString


addressRegister =
    succeed AddressRegister
        |. symbol "["
        |= charChomper isRegister
        |. symbol "]"


isRegister char =
    List.member char [ 'A', 'B', 'C', 'D' ]


register =
    succeed Register
        |= charChomper isRegister


addressConstant =
    succeed AddressConstant
        |. symbol "["
        |= map String.fromInt Parser.int
        |. symbol "]"


charConstant =
    succeed CharConstant
        |. symbol "'"
        |= charChomper isValidChar
        |. symbol "'"


isValidChar char =
    Char.isAlpha char
        || (char == ' ')


constant =
    succeed Constant
        |= map String.fromInt Parser.int


function =
    chompWhile Char.isAlpha
        |> Parser.getChompedString


arguments : Parser Argument
arguments =
    oneOf
        [ register
        , constant
        , backtrackable charConstant
        , backtrackable addressRegister
        , backtrackable addressConstant
        ]


opcodeAirty2 =
    succeed OpcodeAirty2
        |. spaces
        |= function
        |. spaces
        |= arguments
        |. spaces
        |. symbol ","
        |. spaces
        |= arguments
        |. spaces
        |. end


opcodeAirty1 =
    succeed OpcodeAirty1
        |. spaces
        |= function
        |. spaces
        |= arguments
        |. spaces
        |. end


opcodeAirty0 =
    succeed OpcodeAirty0
        |. spaces
        |= function
        |. spaces
        |. end


opcode : Parser Opcodes
opcode =
    oneOf
        [ map A2 <| backtrackable opcodeAirty2
        , map A1 <| backtrackable opcodeAirty1
        , map A0 <| backtrackable opcodeAirty0
        ]


type alias Ram =
    Array Byte


assembleLine string =
    run opcode string


lookup1ArgOpcodes string =
    case string of
        "HLT" ->
            mkByte 0

        "MOV" ->
            mkByte 7

        "INC" ->
            mkByte 18

        _ ->
            mkByte 0


opcodeToBytes : Opcodes -> List Byte
opcodeToBytes opc =
    case opc of
        A0 op0 ->
            [ lookup1ArgOpcodes op0.x ]

        A1 op1 ->
            [ lookup1ArgOpcodes op1.x, argumentToBytes op1.y ]

        A2 op2 ->
            [ lookup1ArgOpcodes op2.x, argumentToBytes op2.y, argumentToBytes op2.z ]


toBytes : String -> List Byte
toBytes string =
    case assembleLine string of
        Ok a ->
            opcodeToBytes a

        Err problems ->
            []


assembleCode string =
    string
        |> String.lines
        |> List.map toBytes
        |> List.concat
