module Byte exposing (Byte(..), byteAdd, mkByte, toHexstring, toInt)

import Hex


type Byte
    = Byte Int


mkByte int =
    Byte (modBy 255 int)


toInt (Byte int) =
    int


byteAdd (Byte a) (Byte b) =
    mkByte (a + b)


toHexstring (Byte int) =
    Hex.toString int
