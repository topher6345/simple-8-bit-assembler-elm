module CPU exposing (CPU, Msg(..), byteToRegisterValue, initalCPU, update)

import Array exposing (Array)
import Byte exposing (..)


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


type Msg
    = --Copies a value from src to dest
      MOV_REG_BYTE Byte Byte


update opcode cpu =
    case opcode of
        MOV_REG_BYTE src (Byte dest) ->
            let
                value =
                    byteToRegisterValue cpu src
            in
            case dest of
                0 ->
                    { cpu | registerA = value, instructionPointer = byteAdd cpu.instructionPointer (Byte 1) }

                1 ->
                    { cpu | registerB = value, instructionPointer = byteAdd cpu.instructionPointer (Byte 1) }

                2 ->
                    { cpu | registerC = value, instructionPointer = byteAdd cpu.instructionPointer (Byte 1) }

                3 ->
                    { cpu | registerD = value, instructionPointer = byteAdd cpu.instructionPointer (Byte 1) }

                _ ->
                    cpu
