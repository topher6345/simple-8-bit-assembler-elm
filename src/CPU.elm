module CPU exposing (CPU, Msg(..), blankRam, fetch, fetchInstruction, initalCPU, initialRam, loadRam, lookupRegister, update, updateAddress, updateRegister)

import Array exposing (Array)
import Byte exposing (..)


type alias CPU =
    { ram : Array Byte
    , registerA : Byte
    , registerB : Byte
    , registerC : Byte
    , registerD : Byte
    , instructionPointer : Byte
    , stackPointer : Byte
    , zeroFlag : Bool
    , carryFlag : Bool
    }


loadRam cpu =
    Array.initialize 256 <| \x -> Maybe.withDefault (mkByte 0) <| Array.get x cpu


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


blankRam =
    Array.initialize 256 <| always <| mkByte 0


initialRam =
    Array.set 232 (mkByte 80) blankRam


initalCPU =
    { ram = initialRam
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
    | MOV_REG_ADDRESS Byte Byte
    | INC_REG Byte
    | MOV_CONST_CHAR_TO_CONST_ADDR Byte Byte
    | HLT


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


updateAddress cpu (Byte address) value =
    Array.set address value cpu.ram


fetch cpu (Byte index) =
    Maybe.withDefault (Byte 0) <| Array.get index cpu.ram


fetchInstruction : CPU -> Byte -> Msg
fetchInstruction cpu (Byte int) =
    case int of
        7 ->
            let
                ip =
                    cpu.instructionPointer

                x =
                    fetch cpu (byteAdd ip (Byte 1))

                y =
                    fetch cpu (byteAdd ip (Byte 2))
            in
            MOV_CONST_CHAR_TO_CONST_ADDR x y

        _ ->
            HLT


update opcode cpu =
    case opcode of
        INC_REG reg ->
            let
                ip =
                    byteAdd cpu.instructionPointer (Byte 2)

                ( sum, carryFlag, zeroFlag ) =
                    carryAdd (lookupRegister cpu reg) (Byte 1)

                model =
                    updateRegister cpu reg sum
            in
            { model | instructionPointer = ip, carryFlag = carryFlag, zeroFlag = zeroFlag }

        MOV_REG_BYTE sourceRegister destinationRegister ->
            let
                ip =
                    byteAdd cpu.instructionPointer (Byte 3)

                model =
                    updateRegister cpu destinationRegister <|
                        lookupRegister cpu sourceRegister
            in
            { model | instructionPointer = ip }

        MOV_REG_ADDRESS sourceRegister destinationAddress ->
            { cpu
                | instructionPointer = byteAdd cpu.instructionPointer (Byte 3)
                , ram =
                    updateAddress cpu destinationAddress <|
                        lookupRegister cpu sourceRegister
            }

        MOV_CONST_CHAR_TO_CONST_ADDR destinationAddress char ->
            { cpu
                | instructionPointer = byteAdd cpu.instructionPointer (Byte 3)
                , ram = updateAddress cpu destinationAddress char
            }

        HLT ->
            cpu
