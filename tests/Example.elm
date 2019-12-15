module Example exposing (suite)

import Byte exposing (..)
import CPU exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "CPU"
        [ test "apply MOV_REG_BYTE" <|
            \_ ->
                let
                    state =
                        { initalCPU | registerA = Byte 1 }

                    actual =
                        update (MOV_REG_BYTE (Byte 0) (Byte 1)) state

                    expected =
                        { initalCPU | registerA = Byte 1, registerB = Byte 1, instructionPointer = Byte 3 }
                in
                actual |> Expect.equal expected
        , test "apply INC_REG by 1" <|
            \_ ->
                let
                    state =
                        { initalCPU | registerA = Byte 1 }

                    actual =
                        update (INC_REG (Byte 0)) state

                    expected =
                        { initalCPU | registerA = Byte 2, instructionPointer = Byte 2 }
                in
                actual |> Expect.equal expected
        , test "apply INC_REG with Carry" <|
            \_ ->
                let
                    state =
                        { initalCPU | registerA = mkByte 255 }

                    actual =
                        update (INC_REG (Byte 0)) state

                    expected =
                        { initalCPU | registerA = Byte 0, instructionPointer = Byte 2, carryFlag = True }
                in
                actual |> Expect.equal expected
        ]
