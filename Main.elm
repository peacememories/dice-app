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


view : Die -> List (Attribute msg) -> Html msg
view value attrs =
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


toInt : Die -> Int
toInt value =
    case value of
        Plus ->
            1

        Minus ->
            -1

        Empty ->
            0


main : Program Never
main =
    Dice.program
        { gen = gen
        , view = view
        , toInt = toInt
        }
