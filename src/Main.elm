module Main exposing (Model, Msg(..), displayBool, initialModel, main, memoryRows, update, view)

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
    | ToggleHexDisplay
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
                cpu =
                    model.cpu

                mem =
                    { cpu
                        | ram =
                            assembleCode model.code
                                |> Array.fromList
                                |> CPU.loadRam
                    }
            in
            { model | cpu = mem }

        Reset ->
            let
                cpu =
                    model.cpu

                mem =
                    { cpu | ram = CPU.blankRam, instructionPointer = mkByte 0 }
            in
            { model | cpu = mem }

        CodeChange string ->
            { model | code = string }

        Step ->
            { model | cpu = CPU.tick model.cpu }

        ToggleHexDisplay ->
            { model | cpuDisplayHex = not model.cpuDisplayHex }


displayBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


showOutput ram =
    let
        f byte =
            span [ style "background-color" "grey" ]
                [ Byte.toInt byte
                    |> Char.fromCode
                    |> String.fromChar
                    |> text
                ]
    in
    Array.slice 232 256 ram
        |> Array.toList
        |> List.map f


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
                    [ td [] [ text <| displayByte model.cpuDisplayHex model.cpu.registerA ]
                    , td [] [ text <| displayByte model.cpuDisplayHex model.cpu.registerB ]
                    , td [] [ text <| displayByte model.cpuDisplayHex model.cpu.registerC ]
                    , td [] [ text <| displayByte model.cpuDisplayHex model.cpu.registerD ]
                    , td [] [ text <| displayByte model.cpuDisplayHex model.cpu.instructionPointer ]
                    , td [] [ text <| displayByte model.cpuDisplayHex model.cpu.stackPointer ]
                    , td [] [ text <| displayBool model.cpu.zeroFlag ]
                    , td [] [ text <| displayBool model.cpu.carryFlag ]
                    , td [] [ text "F" ]
                    ]
                ]
            ]
        , h2 [] [ text "Ram" ]
        , label [] [ text "hex display" ]
        , input [ type_ "checkbox", onClick ToggleHexDisplay ] []
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


memoryRows : Array Byte -> Bool -> List (Html msg)
memoryRows array displayHex =
    let
        row x y =
            Array.slice x y array
                |> Array.map (displayByte displayHex >> cpuByteTd False)
                |> Array.toList
                |> tr []
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


displayByte : Bool -> Byte -> String
displayByte displayHex byte =
    if displayHex then
        byteToHex byte

    else
        byteToDecimal byte


byteToDecimal byte =
    toInt byte
        |> String.fromInt
        |> String.padLeft 3 '0'


byteToHex : Byte -> String
byteToHex byte =
    toInt byte
        |> Hex.toString
        |> String.padLeft 2 '0'
        |> String.toUpper


cpuByteTd : Bool -> String -> Html msg
cpuByteTd selected string =
    td
        [ style "width" "2em"
        , style "text-align" "center"
        , style "background-color" <|
            if selected then
                "yellow"

            else
                "white"
        ]
        [ text string ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
