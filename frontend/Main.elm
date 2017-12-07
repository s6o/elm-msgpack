module Main exposing (..)

import Html exposing (Html)
import Messages exposing (Msg)
import Model exposing (Model)
import Update
import View


{-| Small application to demonstrate MessagePack usage.
-}
main : Program Never Model Msg
main =
    Html.program
        { init = Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        , view = View.view
        }
