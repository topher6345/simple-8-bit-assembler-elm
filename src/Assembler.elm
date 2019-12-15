module Assembler exposing (assemble)

import Array
import Regex


tyle alias RAM =
    Array Byte


assemble : String -> RAM
assemble =
    case string of
        _ ->
            Array.initialize 256 <| always <| mkByte 0


pattern =
    Regex.fromString "[a-z]+"


assembleLine string =
    string



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
