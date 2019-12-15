module Byte exposing (Byte(..), byteAdd, carryAdd, mkByte, toHexstring, toInt)

import Hex


type Byte
    = Byte Int


mkByte int =
    Byte (modBy 256 int)



-- Result, Carry, Zero


carryAdd : Byte -> Byte -> ( Byte, Bool, Bool )
carryAdd (Byte a) (Byte b) =
    let
        sum =
            a + b

        carry =
            a + b > 255

        zero =
            sum == 0
    in
    ( mkByte sum, carry, zero )


toInt (Byte int) =
    int


byteAdd (Byte a) (Byte b) =
    mkByte (a + b)


toHexstring (Byte int) =
    Hex.toString int
