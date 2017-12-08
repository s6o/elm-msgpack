module View
    exposing
        ( view
        )

import Api
import Dict exposing (Dict)
import Hex
import Html exposing (Html, div, h1, h2, hr, input, table, tbody, td, text, th, tr)
import Html.Attributes exposing (colspan, disabled, style, type_, value)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Json.Encode
import Maybe.Extra as EMaybe
import Messages exposing (Msg(..))
import Model exposing (Model)
import MsgPack exposing (MsgPack)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "elm-msgpack demo" ]
        , hr [] []
        , div
            []
            [ h2 [] [ text "MessagePack base types" ]
            , table
                [ style
                    [ ( "padding", "2px" )
                    , ( "width", "100%" )
                    ]
                ]
                [ th [] [ text "GET" ]
                , th [] []
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
                , lazy baseTypesRows model.baseTypes
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


baseTypesRows : Result MsgPack.Error MsgPack -> Html Msg
baseTypesRows msgpack =
    let
        mp =
            msgpack
                |> Result.map (MsgPack.toDict >> Result.withDefault Dict.empty)
                |> Result.withDefault Dict.empty
    in
    tbody
        []
        [ tr
            []
            [ td [ colspan 2 ] [ text "Expected" ]
            , td [] [ text "Received" ]
            ]
        , tr
            []
            [ td [] [ text "binbytes" ]
            , td [] [ text "0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA" ]
            , td
                []
                [ Dict.get "binbytes" mp
                    |> Maybe.map (MsgPack.toBytes >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault []
                    |> List.map (Hex.toString >> String.toUpper >> (++) "0x")
                    |> String.join ", "
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "bool_f" ]
            , td [] [ text "False" ]
            , td
                []
                [ Dict.get "bool_f" mp
                    |> Maybe.map (MsgPack.toBool >> Result.toMaybe)
                    |> EMaybe.join
                    |> (\mv ->
                            case mv of
                                Nothing ->
                                    ""

                                Just v ->
                                    toString v
                       )
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "bool_t" ]
            , td [] [ text "True" ]
            , td
                []
                [ Dict.get "bool_t" mp
                    |> Maybe.map (MsgPack.toBool >> Result.toMaybe)
                    |> EMaybe.join
                    |> (\mv ->
                            case mv of
                                Nothing ->
                                    ""

                                Just v ->
                                    toString v
                       )
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "days" ]
            , td [] [ text "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31" ]
            , td
                []
                [ Dict.get "days" mp
                    |> Maybe.map (MsgPack.toList >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault []
                    |> List.map (MsgPack.toInt >> Result.toMaybe >> Maybe.withDefault 0 >> toString)
                    |> String.join ", "
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "extbytes" ]
            , td [] [ text "0x07, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF" ]
            , td
                []
                [ Dict.get "extbytes" mp
                    |> Maybe.map (MsgPack.toBytes >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault []
                    |> List.map (Hex.toString >> String.toUpper >> (++) "0x")
                    |> String.join ", "
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "fixints" ]
            , td [] [ text "1, 2, 3, 4, 5, 6, 7, 8, 9, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9" ]
            , td
                []
                [ Dict.get "fixints" mp
                    |> Maybe.map (MsgPack.toList >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault []
                    |> List.map (MsgPack.toInt >> Result.toMaybe >> Maybe.withDefault 0 >> toString)
                    |> String.join ", "
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "fixmap" ]
            , td [] [ text "{\"p1\":\"Jane Doe\",\"p2\":\"John Doe\"}" ]
            , td
                []
                [ Dict.get "fixmap" mp
                    |> Maybe.map (MsgPack.toJson >> Result.toMaybe)
                    |> EMaybe.join
                    |> (\mjson ->
                            case mjson of
                                Nothing ->
                                    ""

                                Just value ->
                                    Json.Encode.encode 0 value
                       )
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "floats" ]
            , td [] [ text "1.25, 3.14, -0.25, 0.75" ]
            , td
                []
                [ Dict.get "floats" mp
                    |> Maybe.map (MsgPack.toList >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault []
                    |> List.map (MsgPack.toFloat >> Result.toMaybe >> Maybe.withDefault 0.0 >> toString)
                    |> String.join ", "
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "ints" ]
            , td [] [ text "-2038, -100200, 4096, 72150" ]
            , td
                []
                [ Dict.get "ints" mp
                    |> Maybe.map (MsgPack.toList >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault []
                    |> List.map (MsgPack.toInt >> Result.toMaybe >> Maybe.withDefault 0 >> toString)
                    |> String.join ", "
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "msglong" ]
            , td [] [ text "Once upon a time in a galaxy far far away, yes, far far away ..." ]
            , td
                []
                [ Dict.get "msglong" mp
                    |> Maybe.map (MsgPack.toString >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "msgshort" ]
            , td [] [ text "msgpack" ]
            , td
                []
                [ Dict.get "msgshort" mp
                    |> Maybe.map (MsgPack.toString >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "msgutf" ]
            , td [] [ text "öösel, õun, äädikas - at night, apple, vinegar" ]
            , td
                []
                [ Dict.get "msgutf" mp
                    |> Maybe.map (MsgPack.toString >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "novalue" ]
            , td [] [ text "isNil(v) == True" ]
            , td
                []
                [ Dict.get "novalue" mp
                    |> Maybe.map MsgPack.isNil
                    |> (\mb ->
                            case mb of
                                Nothing ->
                                    ""

                                Just v ->
                                    toString v
                       )
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "x" ]
            , td [] [ text "X" ]
            , td
                []
                [ Dict.get "x" mp
                    |> Maybe.map (MsgPack.toString >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "y" ]
            , td [] [ text "Y" ]
            , td
                []
                [ Dict.get "y" mp
                    |> Maybe.map (MsgPack.toString >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
        , tr
            []
            [ td [] [ text "z" ]
            , td [] [ text "Z" ]
            , td
                []
                [ Dict.get "z" mp
                    |> Maybe.map (MsgPack.toString >> Result.toMaybe)
                    |> EMaybe.join
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
        , tr [] [ td [ colspan 3 ] [ text "JSON" ] ]
        , tr []
            [ td
                [ colspan 3 ]
                [ msgpack
                    |> Result.map (MsgPack.toJson >> Result.withDefault Json.Encode.null)
                    |> (\mjson ->
                            case mjson of
                                Err _ ->
                                    ""

                                Ok value ->
                                    Json.Encode.encode 0 value
                       )
                    |> text
                ]
            ]
        ]
