module Dice exposing (
    Model,
    Msg(..),
    update,
    init,
    view )

import Random as Random
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Html.App as App

type alias Model =
    { dieFace : Int
    , dieRoll : Cmd Msg
    }

type Msg = Roll
         | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Roll -> 
            (model, model.dieRoll)
        NewFace face ->
            ({model | dieFace = face}, Cmd.none)

init : (Model, Cmd Msg)
init =
    let dieRoll = Random.generate NewFace (Random.int -1 1)
    in
        ( { dieFace = 0
          , dieRoll = dieRoll
          }
       , dieRoll
       )

view : Model -> Html Msg
view model =
    button [ class "die", onClick Roll ]
    [ text (toString model.dieFace) ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }
