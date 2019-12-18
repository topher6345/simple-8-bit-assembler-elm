module Example exposing (suite)

import Array exposing (..)
import Assembler exposing (..)
import Byte exposing (..)
import CPU exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "8bit"
        [ describe "CPU"
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
                    Expect.equal expected actual
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
                    Expect.equal expected actual
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
                    Expect.equal expected actual
            , test "apply MOV_REG_ADDRESS" <|
                \_ ->
                    let
                        state =
                            { initalCPU | registerA = mkByte 1 }

                        ram =
                            Array.set 0 (Byte 1) initalCPU.ram

                        actual =
                            update (MOV_REG_ADDRESS (Byte 0) (Byte 0)) state

                        expected =
                            { initalCPU
                                | ram = ram
                                , registerA = Byte 1
                                , instructionPointer = Byte 3
                            }
                    in
                    Expect.equal expected actual
            ]
        , describe "Assembler"
            [ test "arguments 1" <|
                \_ ->
                    let
                        result =
                            case Parser.run arguments "1" of
                                Ok (Constant a) ->
                                    a

                                Ok _ ->
                                    ""

                                Err _ ->
                                    ""
                    in
                    Expect.equal result "1"
            , test "arguments A" <|
                \_ ->
                    let
                        result =
                            case Parser.run arguments "A" of
                                Ok (Register a) ->
                                    a

                                Ok a ->
                                    Debug.toString a

                                Err a ->
                                    Debug.toString a
                    in
                    Expect.equal result "A"
            , test "arguments [A]" <|
                \_ ->
                    let
                        result =
                            case Parser.run arguments "[A]" of
                                Ok (AddressRegister a) ->
                                    a

                                Ok a ->
                                    Debug.toString a

                                Err a ->
                                    Debug.toString a
                    in
                    Expect.equal result "A"
            , test "arguments [1]" <|
                \_ ->
                    let
                        result =
                            case Parser.run arguments "[1]" of
                                Ok (AddressConstant a) ->
                                    a

                                Ok a ->
                                    Debug.toString a

                                Err a ->
                                    Debug.toString a
                    in
                    Expect.equal result "1"
            , test "assembleLine MOV [232], 'h'" <|
                \_ ->
                    let
                        result =
                            case assembleLine "MOV [232], 'h'" of
                                Ok a ->
                                    a

                                Err _ ->
                                    A0 { x = "XXX" }
                    in
                    Expect.equal result (A2 { x = "MOV", y = AddressConstant "232", z = CharConstant "h" })
            , test "bytes out of MOV [232], 'h'" <|
                \_ ->
                    let
                        result =
                            toBytes "MOV [232], 'h'"
                    in
                    Expect.equal result [ Byte 7, Byte 232, Byte 104 ]
            , test "assemble code 2 lines" <|
                \_ ->
                    let
                        result =
                            assembleCode "MOV [232], 'h'\nMOV [233], 'e'"
                    in
                    Expect.equal result [ Byte 7, Byte 232, Byte 104, Byte 7, Byte 233, Byte 101 ]
            ]
        ]
