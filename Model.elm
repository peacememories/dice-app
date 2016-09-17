module Model exposing (Model, Msg(..), Id, Value, update, init)

import Array exposing (Array)
import Random exposing (Generator)
import Updater exposing (Updater, command, simpleCommand, model)

type alias Id = Int
type alias Value = Int

type Msg = RollDie Id
         | AddDie
         | RemoveDie
         | NewRoll Id Value
         | RollAll

type alias Model = Array Value

rng : Generator Value
rng = Random.int -1 1

update : Msg -> Updater Model Msg
update msg =
  (case msg of
    RollDie id -> simpleCommand <| rollDie id
    AddDie -> addDie
    RemoveDie -> model <| removeDie
    NewRoll id value ->
      model (Array.set id value)
    RollAll -> rollAll
  )

rollAll : Updater Model Msg
rollAll = command <| Array.toIndexedList >> List.map (fst >> rollDie) >> Cmd.batch

rollDie : Id -> Cmd Msg
rollDie id = Random.generate (NewRoll id) rng

removeDie : Model -> Model
removeDie model =
  let len = Array.length model
  in
    Array.slice 0 (len-1) model

addDie : Updater Model Msg
addDie model =
  (Array.push 0 model, rollDie (Array.length model))

init : (Model, Cmd Msg)
init =
  Array.repeat 4 0 |> rollAll
