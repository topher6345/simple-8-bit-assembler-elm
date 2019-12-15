module Byte exposing (Byte(..), mkByte, toHexstring, toInt)

import Hex


type Byte
    = Byte Int


mkByte int =
    Byte (modBy 255 int)


toInt (Byte int) =
    int


toHexstring (Byte int) =
    Hex.toString int
