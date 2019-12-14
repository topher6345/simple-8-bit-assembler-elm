module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { count : Int

    -- , registerA : Int
    }


initialModel : Model
initialModel =
    { count = 0 }


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


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        , h2 [] [ text "Registers/Flags" ]
        , table [ style "border" "2px solid blue" ]
            [ thead [ style "border" "2px solid blue" ]
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
            , tbody [ style "border" "2px solid blue" ]
                [ tr
                    []
                    [ td [] [ text "A" ]
                    , td [] [ text "B" ]
                    , td [] [ text "C" ]
                    , td [] [ text "D" ]
                    , td [] [ text "IP" ]
                    , td [] [ text "SP" ]
                    , td [] [ text "Z" ]
                    , td [] [ text "C" ]
                    , td [] [ text "F" ]
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
