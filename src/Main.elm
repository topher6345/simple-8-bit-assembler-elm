module Main exposing (Model, Msg(..), displayBool, displaySelections, findSelections, initialModel, main, memoryRows, update, view)

import Array exposing (Array)
import Assembler exposing (assembleCode)
import Browser
import Browser.Dom as Dom
import Byte exposing (..)
import CPU exposing (CPU)
import Char
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Regex
import Task
import Time


type alias Model =
    { count : Int
    , code : String
    , cpu : CPU
    , flash : String
    , cpuDisplayHex : Bool
    , showEditor : Bool
    , assembled : Bool
    , clockRate : Float
    , running : Bool
    }


initialModel _ =
    ( { count = 0
      , code = "MOV [232], 'h'\nMOV [233], 'e'\nMOV [234], 'l'\nMOV [235], 'l'\nMOV [236], 'o'\nMOV [237], ' '\nMOV [238], 'w'\nMOV [239], 'o'\nMOV [240], 'r'\nMOV [241], 'l'\nMOV [242], 'd'\nHLT"
      , cpu = CPU.initalCPU
      , flash = ""
      , cpuDisplayHex = True
      , showEditor = True
      , assembled = False
      , clockRate = 1000
      , running = False
      }
    , Cmd.none
    )


type Msg
    = Increment
    | Decrement
    | Assemble
    | Reset
    | CodeChange String
    | ToggleHexDisplay
    | Step
    | FocusResult (Result Dom.Error ())
    | ToggleEditor
    | Tick
    | Play
    | ChangeClockFrequency String
    | Pause


focus =
    Task.attempt FocusResult (Dom.focus "code-editor")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model
                | count = model.count + 1
              }
            , Cmd.none
            )

        Decrement ->
            ( { model
                | count = model.count - 1
              }
            , Cmd.none
            )

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
            ( { model
                | cpu = mem
                , assembled = True
              }
            , focus
            )

        Reset ->
            let
                cpu =
                    model.cpu

                mem =
                    { cpu | ram = CPU.blankRam, instructionPointer = mkByte 0 }
            in
            ( { model
                | cpu = mem
                , count = 0
                , assembled = False
              }
            , Cmd.none
            )

        CodeChange string ->
            ( { model
                | code = string
                , assembled = False
              }
            , Cmd.none
            )

        Step ->
            ( { model
                | cpu = CPU.tick model.cpu
                , count = model.count + 1
              }
            , focus
            )

        ToggleHexDisplay ->
            ( { model
                | cpuDisplayHex = not model.cpuDisplayHex
              }
            , Cmd.none
            )

        FocusResult _ ->
            ( model, Cmd.none )

        ToggleEditor ->
            ( { model
                | showEditor = not model.showEditor
              }
            , Cmd.none
            )

        Tick ->
            if model.assembled && model.running then
                ( { model
                    | cpu = CPU.tick model.cpu
                    , count = model.count + 1
                  }
                , focus
                )

            else
                ( model, Cmd.none )

        Play ->
            ( { model
                | running = True
              }
            , Cmd.none
            )

        Pause ->
            ( { model | running = False }
            , Cmd.none
            )

        ChangeClockFrequency freq ->
            let
                clockRate =
                    freq |> String.toFloat |> Maybe.withDefault 0
            in
            ( { model
                | clockRate = clockRate
              }
            , Cmd.none
            )


displayBool bool =
    if bool then
        "true"

    else
        "false"


replaceControlCharacters int =
    if (int > 32) && (int < 127) then
        int

    else
        32


noBreakSpace =
    String.fromChar
        '\u{00A0}'


replaceWhitespaceWithHtmlEntities string =
    if string == " " then
        noBreakSpace

    else
        string


stdoutStyles =
    [ style "display" "inline-block"
    , style "min-width" "1em"
    , style "color" "lightgreen"
    , style "background-color" "black"
    , style "margin" "1px"
    , style "font-size" "2em"
    ]


formatByte byte =
    byte
        |> Byte.toInt
        |> replaceControlCharacters
        |> Char.fromCode
        |> String.fromChar
        |> replaceWhitespaceWithHtmlEntities


showOutput ram =
    let
        viewByte byte =
            pre stdoutStyles
                [ formatByte byte |> text ]
    in
    Array.slice 233 256 ram
        |> Array.toList
        |> List.map viewByte


nullInstructPointer cpu =
    CPU.fetch cpu cpu.instructionPointer == Byte 0


topBarStyles =
    [ style "display" "block"
    , style "width" "100%"
    , style "background-color" "lightgrey"
    ]


editor model =
    [ h2 [] [ text "Code" ]
    , button [ onClick ToggleEditor, style "margin-bottom" "1em" ] [ text "Documentation" ]
    , button [ onClick Assemble, style "margin-bottom" "1em", disabled model.assembled ] [ text "Assemble" ]
    , textarea
        ([ id "code-editor"
         , value model.code
         , onInput CodeChange
         , spellcheck False
         , autofocus True
         , style "width" "100%"
         , style "min-height" "80%"
         , style "font-size" "1.5em"
         ]
            ++ displaySelections model.count model.code
        )
        []
    ]


documentation =
    [ h2 [] [ text "Documentation" ]
    , button [ onClick ToggleEditor, style "margin-bottom" "1em" ] [ text "Code" ]
    , div [ style "line-height" "1.6em" ]
        [ h3 [] [ text "Introduction" ]
        , p [] [ text "This is a tribute to ", a [ href "https://schweigi.github.io/assembler-simulator/index.html", target "_blank" ] [ text "Simple 8-bit Assembler Simulator" ], text " by Marco Schweighauser" ]
        , p []
            [ q [ style "padding" "1em" ]
                [ text "This simulator provides a simplified assembler syntax (based on "
                , a [ href "https://www.nasm.us/", target "_blank" ] [ text "NASM" ]
                , text ") and is simulating a x86-like cpu. In depth documentation and introduction to assembler can be found on the following websites:"
                , ul []
                    [ li [] [ a [ href "http://en.wikipedia.org/wiki/Assembly_language" ] [ text "Assembly - Wikipedia" ] ]
                    , li [] [ a [ href "http://cs.smith.edu/~thiebaut/ArtOfAssembly/artofasm.html" ] [ text "The Art of Assembly Language Programming" ] ]
                    , li [] [ a [ href "http://www.nasm.us/xdoc/2.10.09/html/nasmdoc3.html" ] [ text "NASM Language Documentation" ] ]
                    ]
                , text "The simulator consists of a 8-bit cpu and 256 bytes of memory. All instructions (code) and variables (data) need to fit inside the memory. For simplicity, every instruction (and operand) is 1 byte. Therefore a MOV instruction will use 3 bytes of memory. The simulator provides a console output which is memory mapped from 0xE8 to 0xFF. Memory mapped means that every value written to this memory block is visible on the console."
                ]
            ]
        , h3 [] [ text "Instruction Set" ]
        , h4 [] [ text "MOV - Copy a value" ]
        , p [] [ text "Copies a value from src to dest. The MOV instruction is the only one able to directly modify the memory. SP can be used as operand with MOV." ]
        , pre [] [ text "MOV reg, reg\nMOV reg, address\nMOV reg, constant\nMOV address, reg\nMOV address, constant" ]
        , h4 [] [ text "HLT - Stops the processor. " ]
        , p [] [ text "Stops operation of the processor. Hit Reset button to reset IP before restarting." ]
        , pre [] [ text "HLT" ]
        , p []
            [ a [ href "https://schweigi.github.io/assembler-simulator/instruction-set.html", target "_blank" ] [ text "Original Documentation" ]
            ]
        ]
    ]


view : Model -> Html Msg
view model =
    div [ style "font-family" "Sans-serif" ]
        [ div topBarStyles [ h1 [ style "margin" "0", style "padding" "0.2em" ] [ text "8 Bit Assembler Simulator in Elm" ] ]
        , div [ style "display" "flex", style "flex-direction" "row" ]
            [ div [ style "order" "1", style "min-width" "50%", style "max-width" "50%", style "padding" "10px" ] <|
                if model.showEditor then
                    editor model

                else
                    documentation
            , div [ style "order" "2" ]
                [ h2 [] [ text "CPU" ]
                , button [ onClick Step, disabled (nullInstructPointer model.cpu) ] [ text "Step" ]
                , button [ onClick Reset, disabled <| not model.assembled ] [ text "Reset" ]
                , button [ onClick Play, disabled <| (not model.assembled || model.running) ] [ text "Play" ]
                , select [ onInput ChangeClockFrequency ]
                    [ option [ value "1000" ] [ text "1000" ]
                    , option [ value "2000" ] [ text "2000" ]
                    , option [ value "3000" ] [ text "3000" ]
                    ]
                , button [ onClick Pause, disabled <| not model.running ] [ text "Stop" ]
                , h3 [] [ text "Output" ]
                , div [] [ text model.flash ]
                , div []
                    [ div [] <| showOutput model.cpu.ram
                    ]
                , h3 [] [ text "Registers/Flags" ]
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
                    , tbody [ style "font-family" "Monospace", style "text-align" "center" ]
                        [ tr
                            []
                            [ td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerA ]
                            , td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerB ]
                            , td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerC ]
                            , td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerD ]
                            , td [ style "width" "2em", style "background-color" "orange" ] [ text <| displayByte model.cpuDisplayHex model.cpu.instructionPointer ]
                            , td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.stackPointer ]
                            , td [ style "width" "2em" ] [ text <| displayBool model.cpu.zeroFlag ]
                            , td [ style "width" "2em" ] [ text <| displayBool model.cpu.carryFlag ]
                            , td [ style "width" "2em" ] [ text "F" ]
                            ]
                        ]
                    ]
                , h3 [] [ text "RAM" ]
                , label [] [ text "Decimal display" ]
                , input [ type_ "checkbox", onClick ToggleHexDisplay ] []
                , table [ style "border" "1px solid black", style "font-family" "Monospace" ] <|
                    memoryRows model.cpu.ram model.cpuDisplayHex <|
                        toInt model.cpu.instructionPointer
                ]
            ]
        ]


displaySelections index code =
    let
        ( a, b ) =
            code
                |> findSelections
                |> Array.fromList
                |> Array.get index
                |> Maybe.withDefault ( 0, 0 )
                |> Tuple.mapBoth selectionStart selectionEnd
    in
    [ a, b ]


findSelections code =
    let
        ends =
            String.indexes "\n" code

        incr x =
            x + 1

        tail =
            List.map incr ends

        starts =
            [ 0 ] ++ tail
    in
    List.map2 Tuple.pair starts ends


selectionStart : Int -> Attribute msg
selectionStart position =
    property "selectionStart" (Encode.int position)


selectionEnd : Int -> Attribute msg
selectionEnd position =
    property "selectionEnd" (Encode.int position)


memoryRows : Array Byte -> Bool -> Int -> List (Html msg)
memoryRows array bool ip =
    let
        field index elem =
            displayByte bool elem
                |> cpuByteTd index ip

        row x y =
            array
                |> Array.indexedMap field
                |> Array.slice x y
                |> Array.toList
                |> tr []
    in
    [ row 0 16
    , row 16 32
    , row 32 48
    , row 48 64
    , row 64 80
    , row 80 96
    , row 96 112
    , row 112 128
    , row 128 144
    , row 144 160
    , row 160 176
    , row 176 192
    , row 192 208
    , row 208 224
    , row 224 240
    , row 240 256
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


cpuByteTd : Int -> Int -> String -> Html msg
cpuByteTd index ip string =
    let
        color =
            if index == ip then
                "orange"

            else if index > 232 then
                "lightgrey"

            else
                "seashell"
    in
    td
        [ style "width" "2em"
        , style "text-align" "center"
        , style "background-color" color
        ]
        [ text string ]


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.clockRate (\_ -> Tick)
