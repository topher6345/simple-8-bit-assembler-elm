module Main exposing (Model, Msg(..), displayBool, initialModel, main, mapA, memoryRows, mkByteTd, update, view)

import Array exposing (Array)
import Browser
import Byte exposing (..)
import CPU exposing (CPU)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode


type alias Model =
    { count : Int
    , code : String
    , cpu : CPU
    }


initialModel : Model
initialModel =
    { count = 0
    , code = "; Simple example\n; Writes Hello World to the output\n\n    JMP start\nhello: DB \"Hello World!\" ; Variable\n       DB 0 ; String terminator\n\nstart:\n    MOV C, hello    ; Point to var \n    MOV D, 232  ; Point to output\n    CALL print\n        HLT             ; Stop execution\n\nprint:          ; print(C:*from, D:*to)\n    PUSH A\n    PUSH B\n    MOV B, 0\n.loop:\n    MOV A, [C]  ; Get char from var\n    MOV [D], A  ; Write to output\n    INC C\n    INC D  \n    CMP B, [C]  ; Check if end\n    JNZ .loop   ; jump if not\n\n    POP B\n    POP A\n    RET\n"
    , cpu = CPU.initalCPU
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


displayBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Output" ]
        , h2 [] [ text "Registers/Flags" ]
        , table []
            [ thead []
                [ th [] [ text "A" ]
                , th [] [ text "B" ]
                , th [] [ text "C" ]
                , th [] [ text "D" ]
                , th [] [ text "IP" ]
                , th [] [ text "SP" ]
                , th [] [ text "Z" ]
                , th [] [ text "C" ]
                , th [] [ text "F" ]
                ]
            , tbody []
                [ tr
                    []
                    [ td [] [ text <| toHexstring model.cpu.registerA ]
                    , td [] [ text <| String.fromInt <| toInt model.cpu.registerB ]
                    , td [] [ text <| String.fromInt <| toInt model.cpu.registerC ]
                    , td [] [ text <| String.fromInt <| toInt model.cpu.registerD ]
                    , td [] [ text <| String.fromInt <| toInt model.cpu.instructionPointer ]
                    , td [] [ text <| String.fromInt <| toInt model.cpu.stackPointer ]
                    , td [] [ text <| displayBool model.cpu.zeroFlag ]
                    , td [] [ text <| displayBool model.cpu.carryFlag ]
                    , td [] [ text "F" ]
                    ]
                ]
            ]
        , h2 [] [ text "memory" ]
        , table [] <| memoryRows model.cpu.memory
        , h2 [] [ text "Code" ]
        , textarea
            [ cols 60
            , rows 30
            , value model.code
            ]
            []
        , button [] [ text "Run" ]
        , button [] [ text "Step" ]
        , button [] [ text "Reset" ]
        , button [] [ text "Assemble" ]
        ]


mapA f array =
    Array.toList <| Array.map f array


memoryRows array =
    let
        row x y =
            tr [] <| mapA mkByteTd <| Array.slice x y array
    in
    [ row 0 15
    , row 16 31
    , row 32 47
    , row 48 63
    , row 64 79
    , row 80 95
    , row 96 111
    , row 112 127
    , row 128 143
    , row 144 159
    , row 160 175
    , row 176 191
    , row 192 207
    , row 208 223
    , row 224 239
    , row 240 255
    ]


mkByteTd byte =
    td [] [ text <| String.fromInt <| toInt byte ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
