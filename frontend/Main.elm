module Main exposing (..)

import Hex
import Html exposing (Html, div, text)
import Http
import HttpBuilder exposing (..)
import Meld exposing (Meld)
import MsgPack as MP
import Task exposing (Task)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { apiBase : String
    , loading : Int
    }


type Msg
    = Requests Int (Result Http.Error (Meld Model Http.Error Msg))


init : ( Model, Cmd Msg )
init =
    let
        m =
            Model "/api/v0" 0
    in
    Meld.init m
        |> Meld.withTasks [ msgpackBaseTypes ]
        |> Meld.send Requests (\_ -> m.loading) (\tc -> { m | loading = tc })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Requests taskCount result ->
            case result of
                Ok meld ->
                    Meld.update
                        taskCount
                        (\_ -> model.loading)
                        (\tc -> { model | loading = tc })
                        model
                        meld

                Err httpError ->
                    let
                        _ =
                            Debug.log "Http Error" httpError
                    in
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ text "elm-msgpack-tester"
        ]


requestMsgpackBaseTypes : Model -> Task Http.Error String
requestMsgpackBaseTypes model =
    model.apiBase
        ++ "/msgpack-base-types"
        |> HttpBuilder.get
        |> withExpect Http.expectString
        |> withCacheBuster "_"
        |> HttpBuilder.toTask


msgpackBaseTypes : Meld Model Http.Error Msg -> Task Http.Error (Meld Model Http.Error Msg)
msgpackBaseTypes meld =
    requestMsgpackBaseTypes (Meld.model meld)
        |> Task.map
            (\binstr ->
                let
                    bytes =
                        MP.asBytes binstr

                    msgpack =
                        MP.toMsgPack bytes

                    l1 =
                        Debug.log "Deocded ints"
                            (bytes
                                |> List.map Hex.toString
                            )

                    l2 =
                        Debug.log "MsgPack Result" msgpack
                in
                meld
            )
