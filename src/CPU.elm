module CPU exposing (CPU, initalCPU)

import Array exposing (Array)
import Byte exposing (Byte(..), mkByte)


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


byteToRegisterValue cpu (Byte int) =
    case int of
        0 ->
            cpu.registerA

        1 ->
            cpu.registerB

        2 ->
            cpu.registerC

        3 ->
            cpu.registerD

        _ ->
            mkByte 0


initalCPU =
    { memory = Array.initialize 256 <| always <| mkByte 0
    , registerA = mkByte 0
    , registerB = mkByte 0
    , registerC = mkByte 0
    , registerD = mkByte 0
    , instructionPointer = mkByte 0
    , stackPointer = mkByte 231
    , zeroFlag = False
    , carryFlag = False
    }


type Opcode
    = MOV_REG_BYTE Byte Byte


apply opcode cpu =
    case opcode of
        MOV_REG_BYTE reg byte ->
            cpu
