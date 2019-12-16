module Assembler exposing (Argument(..), OpcodeAirty2, Ram, addressRegister, argParser, assemble, assembleLine, opcode, opcodeAirty1)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, chompIf, chompWhile, commit, float, getChompedString, map, oneOf, problem, run, spaces, succeed, symbol, token)
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


type Argument
    = Constant String
    | AddressRegister String
    | AddressConstant String
    | Register String


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
    succeed Constant |= charChomper Char.isDigit


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


opcodeAirty1 =
    succeed OpcodeAirty1
        |. spaces
        |= (Parser.getChompedString <| chompWhile Char.isAlpha)
        |. spaces
        |= argParser
        |. spaces


opcode : Parser OpcodeAirty2
opcode =
    opcodeAirty2


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
