module View
    exposing
        ( view
        )

import Html exposing (Html, div, text)
import Messages exposing (Msg)
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ text "elm-msgpack-tester"
        ]
