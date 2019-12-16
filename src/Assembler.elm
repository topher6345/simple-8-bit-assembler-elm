module Assembler exposing (OpcodeAirty3, Ram, assemble, assembleLine, foo, pattern, point)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, chompIf, chompWhile, commit, float, getChompedString, map, oneOf, problem, run, spaces, succeed, symbol, token)
import Regex


type alias OpcodeAirty3 =
    { x : String
    , y : Argument
    , z : Argument
    }


type Argument
    = Constant String
    | AddressRegister String
    | AddressConstant String
    | Register String


foo : Parser Argument
foo =
    oneOf
        [ succeed AddressConstant
            |. symbol "["
            |= (Parser.getChompedString <| chompIf Char.isDigit)
            |. symbol "]"
        , succeed Constant
            |= (Parser.getChompedString <| chompIf Char.isDigit)
        , succeed Register
            |= (Parser.getChompedString <| chompIf Char.isAlpha)
        , succeed AddressRegister
            |. symbol "["
            |= (Parser.getChompedString <| chompIf Char.isAlpha)
            |. symbol "]"
        ]


point : Parser OpcodeAirty3
point =
    succeed OpcodeAirty3
        |. spaces
        |= (Parser.getChompedString <| chompWhile Char.isAlpha)
        |. spaces
        |= foo
        |. spaces
        |. symbol ","
        |. spaces
        |= foo
        |. spaces


type alias Ram =
    Array Byte


pattern =
    Regex.fromString "[a-z]+"


assembleLine string =
    run point string



--opToString op =
--    op.x ++ " " ++ op.y ++ " " ++ op.z


assemble : String -> ( String, Ram )
assemble string =
    case assembleLine string of
        Ok a ->
            ( Debug.toString a, CPU.initalCPU.ram )

        Err problems ->
            ( Debug.toString problems, CPU.initalCPU.ram )



-- Split by newlines
-- Parse OPCODE
-- Parse OPCODE A
-- Parse OPCODE A, A
--WHERE A  can be a constant value 42, a register `A`, or a reference location `[42]`
--- Tokens
--lowercase letter
--uppercase letter
-- :
-- ;
-- input = "  MOV C, hello    ; Point to var "
--input = "start:"
--everything_after_semicolon = /;.*/
--token_splitters = /[\s,]+/
--input.lstrip.gsub(everything_after_semicolon, "").rstrip.split(token_splitters)
--
