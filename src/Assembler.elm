module Assembler exposing (Argument(..), OpcodeAirty2, Ram, addressRegister, argParser, assemble, assembleLine, charConstant, keywordRegister, opcode, opcodeAirty0, opcodeAirty1)

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


charChomper f =
    Parser.getChompedString <| chompIf f


keywordRegister =
    succeed identity
        |= oneOf
            [ keyword "A"
            , keyword "B"
            , keyword "C"
            , keyword "D"
            ]


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


charConstant =
    succeed CharConstant
        |. symbol "'"
        |= charChomper Char.isAlpha
        |. symbol "'"


constant =
    succeed Constant |= map String.fromInt Parser.int


argParser : Parser Argument
argParser =
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
        , map A0 <| backtrackable opcodeAirty0
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
