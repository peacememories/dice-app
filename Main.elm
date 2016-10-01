module Main exposing (..)

import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Random exposing (Generator)
import Random.Extra as R
import Dice


type Die
    = Plus
    | Minus
    | Empty


gen : Generator Die
gen =
    R.sample [ Plus, Minus, Empty ]
        |> Random.map (Maybe.withDefault Empty)


view : List (Attribute msg) -> Die -> Html msg
view attrs value =
    let
        ( valName, className ) =
            case value of
                Plus ->
                    ( "+", "plus" )

                Minus ->
                    ( "-", "minus" )

                Empty ->
                    ( "", "empty" )
    in
        H.button
            ([ A.classList
                [ ( "die", True )
                , ( className, True )
                ]
             ]
                ++ attrs
            )
            [ H.text valName ]


sum : List Die -> Int
sum =
    let
        add value aggregate =
            case value of
                Plus ->
                    aggregate + 1

                Minus ->
                    aggregate - 1

                Empty ->
                    aggregate
    in
        List.foldl add 0


init : Die
init =
    Empty


main : Program Never
main =
    Dice.program
        { init = init
        , gen = gen
        , view = view
        , sum = sum
        }
