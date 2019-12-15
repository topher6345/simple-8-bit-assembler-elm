module Assembler exposing (Point, Ram, assemble, assembleLine, pattern, point)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Parser exposing ((|.), (|=), Parser, float, run, spaces, succeed, symbol)
import Regex


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    }


point : Parser Point
point =
    succeed Point
        |. spaces
        |= float
        |. spaces
        |= float
        |. spaces
        |. symbol ","
        |. spaces
        |= float
        |. spaces


type alias Ram =
    Array Byte


pattern =
    Regex.fromString "[a-z]+"


assembleLine string =
    run point string


assemble : String -> ( String, Ram )
assemble string =
    case assembleLine string of
        Ok _ ->
            ( "", CPU.initalCPU.ram )

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
