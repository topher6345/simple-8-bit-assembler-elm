module Main exposing (Model, Msg(..), displayBool, initialModel, main, mapA, memoryRows, mkByteTd, update, view)

import Array exposing (Array)
import Assembler exposing (assembleCode)
import Browser
import Byte exposing (..)
import CPU exposing (CPU)
import Char
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode


type alias Model =
    { count : Int
    , code : String
    , cpu : CPU
    , flash : String
    , cpuDisplayHex : Bool
    }


initialModel : Model
initialModel =
    { count = 0
    , code = "MOV [232], 'h'\nMOV [233], 'e'\nMOV [234], 'l'\nMOV [235], 'l'\nMOV [236], 'o'\nMOV [237], ' '\nMOV [238], 'w'\nMOV [239], 'o'\nMOV [240], 'r'\nMOV [241], 'l'\nMOV [242], 'd'\nHLT"
    , cpu = CPU.initalCPU
    , flash = ""
    , cpuDisplayHex = False
    }


type Msg
    = Increment
    | Decrement
    | Assemble
    | Reset
    | CodeChange String
    | Step


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        Assemble ->
            let
                result =
                    Array.fromList <| assembleCode model.code

                newRam =
                    CPU.loadRam result

                cpu =
                    model.cpu

                mem =
                    { cpu | ram = newRam }
            in
            { model | cpu = mem }

        Reset ->
            let
                cpu =
                    model.cpu

                mem =
                    { cpu | ram = CPU.blankRam }
            in
            { model | cpu = mem }

        CodeChange string ->
            { model | code = string }

        Step ->
            { model | cpu = CPU.tick model.cpu }


displayBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


showOutput ram =
    let
        f byte =
            span [ style "background-color" "grey" ] [ text <| String.fromChar <| Char.fromCode <| Byte.toInt byte ]
    in
    List.map f <| Array.toList <| Array.slice 232 256 ram


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Output" ]
        , div [] [ text model.flash ]
        , div [] <| showOutput model.cpu.ram
        , h2 [] [ text "Registers/Flags" ]
        , table [ style "border" "1px solid black" ]
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
        , h2 [] [ text "Ram" ]
        , table [ style "border" "1px solid black" ] <| memoryRows model.cpu.ram model.cpuDisplayHex
        , h2 [] [ text "Code" ]
        , textarea
            [ cols 60
            , rows 10
            , value model.code
            , onInput CodeChange
            ]
            []
        , button [] [ text "Run" ]
        , button [ onClick Step ] [ text "Step" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Assemble ] [ text "Assemble" ]
        ]


mapA f displayHex array =
    Array.toList <| Array.map f array


memoryRows array displayHex =
    let
        f =
            mapA mkByteTd displayHex

        row x y =
            tr [] <| mapA mkByteTdHex displayHex <| Array.slice x y array
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
    td [ style "width" "2em", style "text-align" "center" ] [ text <| String.fromInt <| toInt byte ]


mkByteTdHex byte =
    td [ style "width" "2em", style "text-align" "center" ] [ text <| String.toUpper <| String.padLeft 2 '0' <| Hex.toString <| toInt byte ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
