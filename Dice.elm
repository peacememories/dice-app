module Dice exposing (program, DiceSpec, Msg)

import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events as E
import Html.App as App
import Random exposing (Generator)
import Array exposing (Array)
import Array.Extra


type alias DiceSpec val =
    { view : List (Attribute (Msg val)) -> val -> Html (Msg val)
    , gen : Generator val
    , sum : List val -> Int
    }


type alias Model val =
    Array val


type alias Id =
    Int


type Msg val
    = Roll Id
    | RollAll
    | NewRoll Id val
    | NewDice (List val)
    | NewDie val
    | AddDie
    | RemoveDie


update :
    DiceSpec val
    -> Msg val
    -> Model val
    -> ( Model val, Cmd (Msg val) )
update spec msg model =
    case msg of
        Roll id ->
            ( model
            , Random.generate (NewRoll id) spec.gen
            )

        NewRoll id value ->
            ( Array.set id value model
            , Cmd.none
            )

        RollAll ->
            init spec (Array.length model)

        AddDie ->
            ( model
            , Random.generate NewDie spec.gen
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


menu : DiceSpec val -> Model val -> Html (Msg val)
menu spec model =
    H.menu []
        [ H.button [ E.onClick RemoveDie ] [ H.text "-" ]
        , H.button [ E.onClick RollAll ]
            [ H.text
                (Array.toList model
                    |> spec.sum
                    |> toString
                )
            ]
        , H.button [ E.onClick AddDie ] [ H.text "+" ]
        ]


view : DiceSpec val -> Model val -> Html (Msg val)
view spec model =
    let
        indexedDie ( id, value ) =
            spec.view [ E.onClick (Roll id) ] value
    in
        H.div []
            [ H.ul
                [ A.class "dice" ]
                (Array.toIndexedList model
                    |> List.map indexedDie
                    |> List.map (\el -> H.li [] [ el ])
                )
            , menu spec model
            ]


init : DiceSpec val -> Int -> ( Model val, Cmd (Msg val) )
init spec count =
    ( Array.empty
    , spec.gen
        |> Random.list count
        |> Random.generate NewDice
    )


program : DiceSpec val -> Program Never
program spec =
    App.program
        { init = init spec 4
        , update = update spec
        , view = view spec
        , subscriptions = (always Sub.none)
        }
