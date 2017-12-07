module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Meld exposing (Error)
import Messages exposing (Msg(..))
import Model exposing (Model)
import MsgPack


init : ( Model, Cmd Msg )
init =
    ( { apiBase = "/api/v0"
      , actions = 0
      , loading = 0
      , baseTypes = Ok MsgPack.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Acts tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.send Results (\_ -> model.actions) (\ac -> { model | actions = ac })

        ActSeqs tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.sequence Results (\_ -> model.actions) (\ac -> { model | actions = ac })

        Requests tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.send Responses (\_ -> model.loading) (\tc -> { model | loading = tc })

        Results actCount result ->
            case result of
                Ok meld ->
                    Meld.update
                        actCount
                        (\_ -> model.actions)
                        (\ac -> { model | actions = ac })
                        model
                        meld

                Err me ->
                    meldError model me

        Responses taskCount result ->
            case result of
                Ok meld ->
                    Meld.update
                        taskCount
                        (\_ -> model.loading)
                        (\tc -> { model | loading = tc })
                        model
                        meld

                Err me ->
                    meldError model me


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


meldError : Model -> Error -> ( Model, Cmd Msg )
meldError model error =
    let
        _ =
            Debug.log "Meld Error" meldError
    in
    ( model, Cmd.none )
