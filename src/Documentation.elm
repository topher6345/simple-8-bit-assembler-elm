module Documentation exposing (documentation)

import Html exposing (..)
import Html.Attributes exposing (..)


documentation =
    [ div
        [ style "line-height" "1.6em"
        , style "width" "100%"
        , style "overflow" "scroll"
        , style "max-height" "calc(100vh - 275px)"
        ]
        [ h3 [] [ text "Introduction" ]
        , p []
            [ text "This is a tribute to "
            , a
                [ href "https://schweigi.github.io/assembler-simulator/index.html"
                , target "_blank"
                ]
                [ text "Simple 8-bit Assembler Simulator" ]
            , text " by Marco Schweighauser"
            ]
        , p []
            [ q [ style "padding" "1em" ]
                [ text "This simulator provides a simplified assembler syntax (based on "
                , a [ href "https://www.nasm.us/", target "_blank" ] [ text "NASM" ]
                , text """
                  ) and is simulating a x86-like cpu.
                  In depth documentation and introduction to
                  assembler can be found on the following websites:
                  """
                , ul []
                    [ li [] [ a [ href "http://en.wikipedia.org/wiki/Assembly_language" ] [ text "Assembly - Wikipedia" ] ]
                    , li [] [ a [ href "http://cs.smith.edu/~thiebaut/ArtOfAssembly/artofasm.html" ] [ text "The Art of Assembly Language Programming" ] ]
                    , li [] [ a [ href "http://www.nasm.us/xdoc/2.10.09/html/nasmdoc3.html" ] [ text "NASM Language Documentation" ] ]
                    ]
                , text """
                    The simulator consists of a 8-bit cpu and 256 bytes of memory.
                    All instructions (code) and variables (data) need to fit inside the memory.
                    For simplicity, every instruction (and operand) is 1 byte.
                    Therefore a MOV instruction will use 3 bytes of memory.
                    The simulator provides a console output which is memory mapped from 0xE8 to 0xFF.
                    Memory mapped means that every value written to this memory block is visible on the console.
                    """
                ]
            ]
        , h3 [] [ text "Instruction Set" ]
        , h4 [] [ text "MOV - Copy a value" ]
        , p [] [ text """
          Copies a value from src to dest.
          The MOV instruction is the only one able to
          directly modify the memory.
          SP can be used as operand with MOV.
              """ ]
        , pre [] [ text "MOV reg, reg\nMOV reg, address\nMOV reg, constant\nMOV address, reg\nMOV address, constant" ]
        , h4 [] [ text "HLT - Stops the processor. " ]
        , p [] [ text "Stops operation of the processor. Hit Reset button to reset IP before restarting." ]
        , pre [] [ text "HLT" ]
        , p []
            [ a [ href "https://schweigi.github.io/assembler-simulator/instruction-set.html", target "_blank" ] [ text "Original Documentation" ]
            ]
        ]
    ]
