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
                        { initalCPU | registerA = Byte 1, registerB = Byte 1, instructionPointer = Byte 1 }
                in
                actual |> Expect.equal expected
        ]
