import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Html.App as App
import Dice as Dice
import Array exposing (Array)

main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }

type alias Model = Array Dice.Model

type Msg = RollAll
         | AddDie
         | RemoveDie
         | DiceMsg Int Dice.Msg

packUpdate : Array (Dice.Model, Cmd Dice.Msg) -> (Model, Cmd Msg)
packUpdate model =
    let conv id (_, cmd) = Cmd.map (DiceMsg id) cmd
    in
        ( Array.map fst model
        , Array.indexedMap conv model |> Array.toList |> Cmd.batch
        )

aggregateResult : Model -> Int
aggregateResult model =
    model |> (Array.map .dieFace) |> (Array.foldr (+) 0)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RollAll ->
            let updated = Array.map (Dice.update Dice.Roll) model
            in
                packUpdate updated
        DiceMsg id msg ->
            case Array.get id model |> Maybe.map (Dice.update msg)
            of
                Just (die, cmd) ->
                    (Array.set id die model, Cmd.map (DiceMsg id) cmd)
                Nothing -> (model, Cmd.none)
        AddDie ->
            let (newDie, cmd) = Dice.init
            in
                (Array.push newDie model
                , Cmd.map (model |> Array.length |> DiceMsg) cmd
                )
        RemoveDie ->
            let len = Array.length model
            in
                if len > 4
                then
                    ( Array.slice 0 (len-1) model
                    , Cmd.none
                    )
                else
                    (model, Cmd.none)

init : (Model, Cmd Msg)
init =
    let inits = Array.repeat 4 Dice.init
    in
        packUpdate inits

view : Model -> Html Msg
view model =
    body []
    [ button [ onClick RemoveDie, disabled (Array.length model <= 4) ]
        [ text "-" ]
    , ul [ class "dicelist" ]
        (Array.indexedMap dieList model |> Array.toList)
    , button [onClick AddDie ]
        [ text "+" ]
    , button [ onClick RollAll ]
        [text (toString (aggregateResult model)) ]
    ]
    

dieList : Int -> Dice.Model -> Html Msg
dieList id model =
    li [] [Dice.view model |> App.map (DiceMsg id)]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none