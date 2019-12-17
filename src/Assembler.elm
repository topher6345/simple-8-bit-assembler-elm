module Assembler exposing (Argument(..), OpcodeAirty2, Ram, addressRegister, argParser, assemble, assembleLine, opcode, opcodeAirty0, opcodeAirty1)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, chompIf, chompWhile, commit, end, float, getChompedString, loop, map, oneOf, problem, run, spaces, succeed, symbol, token)
import Regex


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
    | Register String


type Opcodes
    = A0 OpcodeAirty0
    | A1 OpcodeAirty1
    | A2 OpcodeAirty2


charChomper f =
    Parser.getChompedString <| chompIf f


addressRegister =
    succeed AddressRegister
        |. symbol "["
        |= charChomper Char.isAlpha
        |. symbol "]"


register =
    succeed Register
        |= charChomper Char.isAlpha


addressConstant =
    succeed AddressConstant
        |. symbol "["
        |= charChomper Char.isDigit
        |. symbol "]"


constant =
    succeed Constant |= map String.fromInt Parser.int


argParser : Parser Argument
argParser =
    oneOf
        [ register
        , constant
        , backtrackable addressRegister
        , addressConstant
        ]


opcodeAirty2 =
    succeed OpcodeAirty2
        |. spaces
        |= (Parser.getChompedString <| chompWhile Char.isAlpha)
        |. spaces
        |= argParser
        |. spaces
        |. symbol ","
        |. spaces
        |= argParser
        |. spaces
        |. end


opcodeAirty1 =
    succeed OpcodeAirty1
        |. spaces
        |= (Parser.getChompedString <| chompWhile Char.isAlpha)
        |. spaces
        |= argParser
        |. spaces
        |. end


opcodeAirty0 =
    succeed OpcodeAirty0
        |. spaces
        |= (Parser.getChompedString <| chompWhile Char.isAlpha)
        |. spaces
        |. end


opcode : Parser Opcodes
opcode =
    oneOf
        [ map A2 <| backtrackable opcodeAirty2
        , map A1 <| backtrackable opcodeAirty1
        , map A0 opcodeAirty0
        ]


type alias Ram =
    Array Byte


assembleLine string =
    run opcode string


assemble : String -> ( String, Ram )
assemble string =
    case assembleLine string of
        Ok a ->
            ( Debug.toString a, CPU.initalCPU.ram )

        Err problems ->
            ( Debug.toString problems, CPU.initalCPU.ram )
