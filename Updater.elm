module Updater exposing (Updater, model, command, simpleCommand, batch, (=>>), none)

type alias Updater model msg = model -> (model, Cmd msg)

model : (a -> a) -> Updater a msg
model f value =
  (f value, Cmd.none)

command : (a -> Cmd msg) -> Updater a msg
command f value =
  (value, f value)

simpleCommand : Cmd msg -> Updater a msg
simpleCommand = always >> command

batch : List (Updater a msg) -> Updater a msg
batch list =
  \model ->
    let (newModel, cmdList) = List.foldl combine (model, []) list
    in
      (newModel, Cmd.batch cmdList)

combine : Updater a msg -> (a, List (Cmd msg)) -> (a, List (Cmd msg))
combine updater base =
  let (oldValue, cmdList) = base
      (newValue, cmd) = updater oldValue
  in
    (newValue, cmd :: cmdList)

(=>>) : Updater a msg -> Updater a msg -> Updater a msg
(=>>) left right =
  \model ->
    let (newModel1, cmd1) = left model
        (newModel2, cmd2) = right newModel1
    in
      (newModel2, Cmd.batch [cmd1, cmd2])

none : Updater a msg
none = model identity
