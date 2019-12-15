module CPU exposing (CPU, Msg(..), initalCPU, lookupRegister, update)

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


lookupRegister cpu (Byte int) =
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
    | INC_REG Byte


updateRegister cpu (Byte register) value =
    case register of
        0 ->
            { cpu | registerA = value }

        1 ->
            { cpu | registerB = value }

        2 ->
            { cpu | registerC = value }

        3 ->
            { cpu | registerD = value }

        _ ->
            cpu


update opcode cpu =
    case opcode of
        INC_REG reg ->
            let
                ip =
                    byteAdd cpu.instructionPointer (Byte 2)

                ( sum, carry ) =
                    carryAdd (lookupRegister cpu reg) (Byte 1)

                model =
                    updateRegister cpu reg sum
            in
            { model | instructionPointer = ip, carryFlag = carry }

        MOV_REG_BYTE sourceRegister destinationRegister ->
            let
                ip =
                    byteAdd cpu.instructionPointer (Byte 3)

                model =
                    updateRegister cpu destinationRegister <|
                        lookupRegister cpu sourceRegister
            in
            { model | instructionPointer = ip }
