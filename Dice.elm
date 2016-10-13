module Dice exposing (program, DiceSpec)

import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events as E
import Html.App as App
import Random exposing (Generator)
import Array exposing (Array)
import Array.Extra


type alias Element msg =
    List (Attribute msg) -> Html msg


type alias DiceSpec val =
    { view : val -> Element Msg
    , gen : Generator val
    , toInt : val -> Int
    }


type alias Die =
    { view : Element Msg
    , value : Int
    }


type alias Model =
    Array Die


type alias Id =
    Int


type Msg
    = Roll Id
    | RollAll
    | NewRoll Id Die
    | NewDice (List Die)
    | NewDie Die
    | AddDie
    | RemoveDie


update :
    Generator Die
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update gen msg model =
    case msg of
        Roll id ->
            ( model
            , Random.generate (NewRoll id) gen
            )

        NewRoll id value ->
            ( Array.set id value model
            , Cmd.none
            )

        RollAll ->
            ( model
            , model
                |> Array.length
                |> roll gen
            )

        AddDie ->
            ( model
            , Random.generate NewDie gen
            )

        RemoveDie ->
            (let
                newSize =
                    (Array.length model) - 1
             in
                ( Array.Extra.removeAt newSize model
                , Cmd.none
                )
            )

        NewDice newDice ->
            ( Array.fromList newDice, Cmd.none )

        NewDie value ->
            ( Array.push value model, Cmd.none )


sum : List Die -> Int
sum =
    List.map .value
        >> List.foldl (+) 0


menu : Model -> Html Msg
menu model =
    H.menu []
        [ H.button [ E.onClick RemoveDie ] [ H.text "-" ]
        , H.button [ E.onClick RollAll ]
            [ H.text
                (model
                    |> Array.toList
                    |> sum
                    |> toString
                )
            ]
        , H.button [ E.onClick AddDie ] [ H.text "+" ]
        ]


view : Model -> Html Msg
view model =
    let
        indexedDie ( id, die ) =
            die.view [ E.onClick (Roll id) ]
    in
        H.div []
            [ H.ul
                [ A.class "dice" ]
                (model
                    |> Array.toIndexedList
                    |> List.map indexedDie
                    |> List.map (\el -> H.li [] [ el ])
                )
            , menu model
            ]

roll : Generator Die -> Int -> Cmd Msg
roll gen count =
    gen
        |> Random.list count
        |> Random.generate NewDice

init : Generator Die -> Int -> ( Model, Cmd Msg )
init gen count =
    ( Array.empty
    , roll gen count
    )


program : DiceSpec val -> Program Never
program spec =
    let
        die value =
            { view = spec.view value
            , value = spec.toInt value
            }
    in
        spec.gen
            |> Random.map die
            |> program_


program_ : Generator Die -> Program Never
program_ gen =
    App.program
        { init = init gen 4
        , update = update gen
        , view = view
        , subscriptions = (always Sub.none)
        }
