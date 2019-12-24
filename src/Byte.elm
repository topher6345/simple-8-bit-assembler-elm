module Byte exposing (Byte(..), byteAdd, carryAdd, mkByte, toHexstring, toInt)

import Hex


type Byte
    = Byte Int


mkByte int =
    Byte (modBy 256 int)


carryAdd : Byte -> Byte -> ( Byte, Bool, Bool )
carryAdd (Byte a) (Byte b) =
    let
        sum =
            a + b
    in
    ( mkByte sum, sum > 255, sum == 0 )


toInt (Byte int) =
    int


byteAdd (Byte a) (Byte b) =
    mkByte (a + b)


toHexstring (Byte int) =
    Hex.toString int
