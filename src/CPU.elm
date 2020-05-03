module CPU exposing (CPU, Msg(..), blankRam, fetch, fetchInstruction, initalCPU, initialRam, loadRam, lookupRegister, tick, update, updateAddress, updateRegister)

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
    let
        constructor x =
            cpu
                |> Array.get x
                |> Maybe.withDefault (mkByte 0)
    in
    Array.initialize 256 constructor


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
    mkByte 0
        |> always
        |> Array.initialize 256


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
      MovRegByte Byte Byte
    | MovRegAddress Byte Byte
    | IncrReg Byte
    | MovConstCharToConstAddress Byte Byte
    | Hlt
    | Jump Byte
    | MovConstToRegister Byte Byte
    | Call Byte
    | PushRegister Byte


updateRegister cpu (Byte registerByte) value =
    case registerByte of
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
    Array.set (address + 1) value cpu.ram


tick : CPU -> CPU
tick cpu =
    let
        ip =
            fetch cpu cpu.instructionPointer

        instruction =
            fetchInstruction cpu ip
    in
    update instruction cpu


fetch : CPU -> Byte -> Byte
fetch cpu (Byte index) =
    Array.get index cpu.ram
        |> Maybe.withDefault (Byte 0)


fetchInstruction : CPU -> Byte -> Msg
fetchInstruction cpu (Byte instructionByte) =
    let
        ip =
            cpu.instructionPointer

        x =
            fetch cpu (byteAdd ip (Byte 1))

        y =
            fetch cpu (byteAdd ip (Byte 2))
    in
    case instructionByte of
        7 ->
            MovConstCharToConstAddress x y

        18 ->
            IncrReg x

        31 ->
            Jump x

        6 ->
            MovConstToRegister x y

        56 ->
            Call x

        50 ->
            PushRegister x

        _ ->
            Hlt


update : Msg -> CPU -> CPU
update opcode cpu =
    case opcode of
        IncrReg reg ->
            let
                ( sum, carryFlag, zeroFlag ) =
                    carryAdd (lookupRegister cpu reg) (Byte 1)

                model =
                    updateRegister cpu reg sum
            in
            { model
                | instructionPointer = byteAdd cpu.instructionPointer (Byte 2)
                , carryFlag = carryFlag
                , zeroFlag = zeroFlag
            }

        MovRegByte sourceRegister destinationRegister ->
            let
                ip =
                    byteAdd cpu.instructionPointer (Byte 3)

                model =
                    lookupRegister cpu sourceRegister
                        |> updateRegister cpu destinationRegister
            in
            { model | instructionPointer = ip }

        MovRegAddress sourceRegister destinationAddress ->
            { cpu
                | instructionPointer = byteAdd cpu.instructionPointer (Byte 3)
                , ram =
                    lookupRegister cpu sourceRegister
                        |> updateAddress cpu destinationAddress
            }

        MovConstCharToConstAddress destinationAddress char ->
            { cpu
                | instructionPointer = byteAdd cpu.instructionPointer (Byte 3)
                , ram = updateAddress cpu destinationAddress char
            }

        Jump byte ->
            { cpu
                | instructionPointer = byte
            }

        MovConstToRegister registerNumber constant ->
            let
                foo =
                    updateRegister cpu registerNumber constant
            in
            { foo
                | instructionPointer = byteAdd cpu.instructionPointer (Byte 3)
            }

        Call byte ->
            { cpu
                | instructionPointer = byte
                , stackPointer = byteSub cpu.stackPointer (Byte 1)
                , ram = updateAddress cpu cpu.stackPointer (byteSub byte (Byte 1))
            }

        PushRegister registerByte ->
            { cpu
                | instructionPointer = byteAdd cpu.instructionPointer (Byte 2)
                , stackPointer = byteSub cpu.stackPointer (Byte 1)
                , ram = updateAddress cpu cpu.stackPointer (lookupRegister cpu registerByte)
            }

        Hlt ->
            cpu
