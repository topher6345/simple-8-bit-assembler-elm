module Byte exposing (Byte, mkByte, toInt)


type Byte
    = Byte Int


mkByte int =
    Byte (modBy 255 int)


toInt (Byte int) =
    int
