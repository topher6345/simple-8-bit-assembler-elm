module Main exposing (main)

import Array exposing (..)
import Browser
import Byte exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode


type alias Model =
    { count : Int
    , code : String
    , memory : Array Byte
    , registerA : Byte
    , registerB : Byte
    , registerC : Byte
    , registerD : Byte
    , instructionPointer : Byte
    , stackPointer : Byte
    , zeroFlag : Bool

    --, carryFlag : Bool
    }


initialModel : Model
initialModel =
    { count = 0
    , memory = initialize 256 (always mkByte 0)
    , code = "; Simple example\n; Writes Hello World to the output\n\n    JMP start\nhello: DB \"Hello World!\" ; Variable\n       DB 0 ; String terminator\n\nstart:\n    MOV C, hello    ; Point to var \n    MOV D, 232  ; Point to output\n    CALL print\n        HLT             ; Stop execution\n\nprint:          ; print(C:*from, D:*to)\n    PUSH A\n    PUSH B\n    MOV B, 0\n.loop:\n    MOV A, [C]  ; Get char from var\n    MOV [D], A  ; Write to output\n    INC C\n    INC D  \n    CMP B, [C]  ; Check if end\n    JNZ .loop   ; jump if not\n\n    POP B\n    POP A\n    RET\n"
    , registerA = mkByte 127
    , registerB = mkByte 127
    , registerC = mkByte 254
    , registerD = mkByte 254
    , instructionPointer = mkByte 0
    , stackPointer = mkByte 231
    , zeroFlag = False
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
        [ h2 [] [ text "Registers/Flags" ]
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
                    [ td [] [ text <| toHexstring model.registerA ]
                    , td [] [ text <| String.fromInt <| toInt model.registerB ]
                    , td [] [ text <| String.fromInt <| toInt model.registerC ]
                    , td [] [ text <| String.fromInt <| toInt model.registerD ]
                    , td [] [ text <| String.fromInt <| toInt model.instructionPointer ]
                    , td [] [ text <| String.fromInt <| toInt model.stackPointer ]
                    , td [] [ text <| displayBool model.zeroFlag ]
                    , td [] [ text "C" ]
                    , td [] [ text "F" ]
                    ]
                ]
            ]
        , h2 [] [ text "memory" ]
        , table []
            (foo
                model
            )
        , h2 [] [ text "Code" ]
        , textarea
            [ cols 60
            , rows 30
            , value model.code
            ]
            []
        , h2 [] [ text "Output" ]
        , button [] [ text "Run" ]
        , button [] [ text "Step" ]
        , button [] [ text "Reset" ]
        , button [] [ text "Assemble" ]
        ]


foo model =
    Array.toList <|
        Array.map
            (\i -> td [] [ text <| String.fromInt <| toInt i ])
            model.memory


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
