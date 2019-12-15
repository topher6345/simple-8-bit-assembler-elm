module Byte exposing (Byte(..), byteAdd, carryAdd, mkByte, toHexstring, toInt)

import Hex


type Byte
    = Byte Int


mkByte int =
    Byte (modBy 256 int)


carryAdd (Byte a) (Byte b) =
    if a + b > 255 then
        ( mkByte (a + b), True )

    else
        ( mkByte (a + b), False )


toInt (Byte int) =
    int


byteAdd (Byte a) (Byte b) =
    mkByte (a + b)


toHexstring (Byte int) =
    Hex.toString int
