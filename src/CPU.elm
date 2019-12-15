module CPU exposing (CPU)

import Array exposing (Array)
import Byte exposing (Byte)


type alias CPU =
    { memory : Array Byte
    , registerA : Byte
    , registerB : Byte
    , registerC : Byte
    , registerD : Byte
    , instructionPointer : Byte
    , stackPointer : Byte
    , zeroFlag : Bool
    , carryFlag : Bool
    }
