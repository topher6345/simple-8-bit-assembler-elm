port module Main exposing (Model, Msg(..))

import Array exposing (Array)
import Assembler exposing (assembleCode)
import Browser
import Browser.Dom as Dom
import Byte exposing (..)
import CPU exposing (CPU)
import Char
import Dict exposing (Dict)
import Documentation
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init
import Json.Decode exposing (Decoder, decodeString, field, int, list, string)
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
    , mapping : Dict String Int
    , labels : Dict String Int
    , editing : Bool
    }


initialModel _ =
    ( { count = 0
      , code = Init.program
      , cpu = CPU.initalCPU
      , flash = ""
      , cpuDisplayHex = True
      , showEditor = True
      , assembled = False
      , clockRate = 200
      , running = False
      , mapping = Dict.fromList []
      , labels = Dict.fromList []
      , editing = True
      }
    , Cmd.none
    )


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ messageReceiver Receive
        , Time.every model.clockRate (\_ -> Tick)
        ]


type alias Artifacts =
    { code : Array String
    , mapping : String
    , label : Array String
    }


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
    | Receive String


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
            ( model, sendMessage model.code )

        Receive mes ->
            let
                cpu =
                    model.cpu

                mem =
                    { cpu
                        | ram =
                            codeDecoder mes
                                |> List.map mkByte
                                |> Array.fromList
                                |> CPU.loadRam
                    }
            in
            ( { model
                | cpu = mem
                , assembled = True
                , mapping = mappingDecoder mes
              }
            , focus
            )

        Reset ->
            let
                cpu =
                    model.cpu

                mem =
                    { cpu
                        | ram = CPU.blankRam
                        , instructionPointer = mkByte 0
                        , stackPointer = mkByte 231
                    }
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
                , editing = True
              }
            , Cmd.none
            )

        Step ->
            ( { model
                | cpu = CPU.tick model.cpu
                , count = model.count + 1
                , editing = False
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
                , editing = False
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


stdOutOffset =
    232


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
            pre [] [ text <| formatByte byte ]
    in
    Array.slice stdOutOffset 256 ram
        |> Array.toList
        |> List.map viewByte


nullInstructPointer cpu =
    CPU.fetch cpu cpu.instructionPointer == Byte 0


topBarStyles =
    [ style "display" "block"
    , style "width" "100%"
    , style "background-color" "lightgrey"
    , style "max-height" "50px"
    ]


editor model =
    [ h2 [] [ text "Code" ]
    , button [ onClick ToggleEditor, style "margin-bottom" "1em" ] [ text "Documentation" ]
    , button [ onClick Assemble, style "margin-bottom" "1em", disabled model.assembled ] [ text "Assemble" ]
    , div [ style "width" "100%" ]
        [ textarea
            ([ id "code-editor"
             , value model.code
             , onInput CodeChange
             , spellcheck False
             , autofocus True
             , style "min-width" "calc(100% - 0.5em)"
             , style "max-height" "calc(100vh - 300px)"
             , style "min-height" "calc(100vh - 300px)"
             , style "font-size" "1.5em"
             , style "padding" "0.5em"
             ]
                ++ (if model.editing then
                        [ property "selectionStart"
                            (Encode.int 0)
                        , property
                            "selectionEnd"
                            (Encode.int 0)
                        ]

                    else
                        displaySelections (Byte.toInt model.cpu.instructionPointer) model.code model.mapping
                   )
            )
            []
        ]
    ]


documentationNavigation =
    [ h2 [] [ text "Documentation" ]
    , button [ onClick ToggleEditor, style "margin-bottom" "1em" ] [ text "Code" ]
    ]
        ++ Documentation.documentation


codeDecoder string =
    let
        listDecoder =
            Json.Decode.list Json.Decode.int
    in
    case decodeString (field "code" listDecoder) string of
        Ok res ->
            res

        Err _ ->
            []


mappingDecoder string =
    let
        listDecoder =
            Json.Decode.dict Json.Decode.int
    in
    case decodeString (field "mapping" listDecoder) string of
        Ok res ->
            res

        Err _ ->
            Dict.fromList []


view : Model -> Html Msg
view model =
    div
        []
        [ div []
            [ h1
                []
                [ text "8 Bit Assembler Simulator "
                , small []
                    [ a
                        [ href "https://github.com/topher6345/simple-8-bit-assembler-elm " ]
                        [ text "source" ]
                    ]
                ]
            ]
        , div
            [ style "display" "flex"
            , style "min-height" "calc(50px - 100vh)"
            ]
            [ div
                [ style "order" "1"
                , style "min-width" "50%"
                , style "max-width" "50%"
                , style "padding" "0 1em"
                ]
              <|
                if model.showEditor then
                    editor model

                else
                    documentationNavigation
            , div
                [ style "order" "2"
                , style "padding" "0 1em"
                ]
                [ h2 [] [ text "CPU" ]
                , div [] [ button [ onClick Reset, disabled <| not model.assembled ] [ text "Reset" ] ]
                , button [ onClick Step, disabled (nullInstructPointer model.cpu) ] [ text "Step" ]
                , button
                    [ onClick Play
                    , disabled <|
                        (not model.assembled
                            || model.running
                            || nullInstructPointer model.cpu
                        )
                    ]
                    [ text "Play" ]
                , label [] [ text "clock speed" ]
                , select [ onInput ChangeClockFrequency ]
                    [ option [ value "200" ] [ text "200" ]
                    , option [ value "500" ] [ text "500" ]
                    , option [ value "1000" ] [ text "1000" ]
                    , option [ value "2000" ] [ text "2000" ]
                    , option [ value "3000" ] [ text "3000" ]
                    ]
                , text "ms"
                , button [ onClick Pause, disabled <| not model.running || nullInstructPointer model.cpu ] [ text "Stop" ]
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
                    , tbody
                        [ style "font-family" "Monospace"
                        , style "text-align" "center"
                        ]
                        [ tr
                            []
                            [ td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerA ]
                            , td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerB ]
                            , td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerC ]
                            , td [ style "width" "2em" ] [ text <| displayByte model.cpuDisplayHex model.cpu.registerD ]
                            , td [ style "width" "2em", style "background-color" "lightblue" ] [ text <| displayByte model.cpuDisplayHex model.cpu.instructionPointer ]
                            , td [ style "width" "2em", style "background-color" "coral" ] [ text <| displayByte model.cpuDisplayHex model.cpu.stackPointer ]
                            , td [ style "width" "2em" ] [ text <| displayBool model.cpu.zeroFlag ]
                            , td [ style "width" "2em" ] [ text <| displayBool model.cpu.carryFlag ]
                            , td [ style "width" "2em" ] [ text "F" ]
                            ]
                        ]
                    ]
                , h3 [] [ text "RAM" ]
                , label [] [ text "Decimal display" ]
                , input [ type_ "checkbox", onClick ToggleHexDisplay ] []
                , table
                    [ style "border" "1px solid black"
                    , style "font-family" "Monospace"
                    ]
                  <|
                    memoryRows model.cpu.ram
                        model.cpuDisplayHex
                        (toInt model.cpu.instructionPointer)
                        (toInt model.cpu.stackPointer)
                ]
            ]
        ]


displaySelections : Int -> String -> Dict String Int -> List (Attribute msg)
displaySelections index code mapping =
    let
        mapper i =
            Maybe.withDefault 1 <| Dict.get (String.fromInt index) mapping

        ( a, b ) =
            code
                |> findSelections
                |> Array.fromList
                |> Array.get (mapper index)
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


memoryRows :
    Array Byte
    -> Bool
    -> Int
    -> Int
    -> List (Html msg)
memoryRows array bool ip sp =
    let
        field index elem =
            displayByte bool elem
                |> cpuByteTd index ip sp

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


cpuByteTd :
    Int
    -> Int
    -> Int
    -> String
    -> Html msg
cpuByteTd index ip sp string =
    let
        color =
            if index == ip then
                "lightblue"

            else if index == sp then
                "coral"

            else if sp < stdOutOffset && index > sp && index < stdOutOffset then
                "orange"

            else if index >= stdOutOffset then
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
