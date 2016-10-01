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
    , init : val
    }


type alias Model val =
    Array val


type alias Id =
    Int


type Msg val
    = Roll Id
    | RollAll
    | NewRoll Id val
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
            ( Array.Extra.update id (always value) model
            , Cmd.none
            )

        RollAll ->
            ( model
            , let
                indexGen id =
                    Random.generate (NewRoll id) spec.gen
              in
                Array.initialize (Array.length model) indexGen
                    |> Array.toList
                    |> Cmd.batch
            )

        AddDie ->
            let
                newSize =
                    (Array.length model) + 1
            in
                ( Array.Extra.resizelRepeat newSize spec.init model
                , Cmd.none
                )

        RemoveDie ->
            let
                newSize =
                    (Array.length model) - 1
            in
                ( Array.Extra.resizelRepeat newSize spec.init model
                , Cmd.none
                )


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


init : DiceSpec val -> ( Model val, Cmd (Msg val) )
init spec =
    ( Array.repeat 4 spec.init, Cmd.none )


program : DiceSpec val -> Program Never
program spec =
    App.program
        { init = init spec
        , update = update spec
        , view = view spec
        , subscriptions = (always Sub.none)
        }
