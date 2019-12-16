module Assembler exposing (Point, Ram, assemble, assembleLine, keyword, pattern, point)

import Array exposing (Array)
import Byte exposing (Byte, mkByte)
import CPU
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, chompIf, chompWhile, commit, float, getChompedString, map, oneOf, problem, run, spaces, succeed, symbol, token)
import Regex


type alias Point =
    { x : String
    , y : String
    , z : String
    }


keyword : String -> Parser ()
keyword kwd =
    succeed identity
        |. backtrackable (token kwd)
        |= oneOf
            [ map (\_ -> True) (backtrackable (chompIf isVarChar))
            , succeed False
            ]
        |> andThen (checkEnding kwd)


checkEnding : String -> Bool -> Parser ()
checkEnding kwd isBadEnding =
    if isBadEnding then
        problem ("expecting the `" ++ kwd ++ "` keyword")

    else
        commit ()


isVarChar : Char -> Bool
isVarChar char =
    Char.isAlphaNum char || char == '_'


point : Parser Point
point =
    succeed Point
        |. spaces
        |= (Parser.getChompedString <| chompWhile Char.isAlpha)
        |. spaces
        |= oneOf
            [ Parser.getChompedString <| chompWhile Char.isAlpha
            , succeed identity
                |. symbol "["
                |= (Parser.getChompedString <| chompWhile Char.isDigit)
                |. symbol "]"
            , Parser.getChompedString <| chompWhile Char.isDigit
            , succeed identity
                |. symbol "["
                |= (Parser.getChompedString <| chompWhile Char.isAlpha)
                |. symbol "]"
            ]
        |. spaces
        |. symbol ","
        |. spaces
        |= (Parser.getChompedString <| chompWhile Char.isAlpha)
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
--
