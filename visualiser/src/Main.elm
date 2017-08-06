module Main exposing (..)

import Visualiser exposing (..)
import Html


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
