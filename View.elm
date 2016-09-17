module View exposing (..)

import Model exposing (..)
import Html exposing (Html, button, text, ul, li, body)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Array

viewDie : Id -> Value -> Html Msg
viewDie id value =
  button [ onClick (RollDie id), class "die" ]
  [ text <| toString value ]

sum : Model -> Value
sum = Array.foldl (+) 0

view : Model -> Html Msg
view model =
  body []
  [ button [onClick RemoveDie ] [ text "-" ]
  , ul [ class "dice" ]
      (Array.toIndexedList model
      |> List.map (uncurry viewDie)
      |> List.map (\el -> li [] [el]))
  , button [ onClick AddDie ] [ text "+" ]
  , button [ onClick RollAll ] [ sum model |> toString |> text ]
  ]
