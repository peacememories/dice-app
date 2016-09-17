import Html.App exposing (program)
import Model exposing (Model, Msg(..), update, init)
import View exposing (view)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program Never
main = program { init = init
               , update = update
               , view = view
               , subscriptions = subscriptions
               }
