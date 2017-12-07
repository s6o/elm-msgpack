module View
    exposing
        ( view
        )

import Api
import Html exposing (Html, div, h1, h2, hr, input, table, tbody, td, text, th, tr)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "elm-msgpack demo" ]
        , hr [] []
        , div
            []
            [ h2 [] [ text "MessagePack base types" ]
            , table
                []
                [ th [] [ text "GET" ]
                , th
                    []
                    [ input
                        [ type_ "button"
                        , Requests [ Api.msgpackBaseTypes ]
                            |> onClick
                        , Model.isLoading model |> disabled
                        , Model.isLoading model |> loadLabel |> value
                        ]
                        []
                    ]
                , tbody
                    []
                    [ tr
                        []
                        [ td [] [ text "" ]
                        ]
                    ]
                ]
            ]
        ]


loadLabel : Bool -> String
loadLabel flag =
    case flag of
        False ->
            "Load"

        True ->
            "Loading ..."
