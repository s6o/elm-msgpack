module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Api
import Meld
import Messages exposing (Msg(..))
import Model exposing (Model)


init : ( Model, Cmd Msg )
init =
    let
        m =
            Model "/api/v0" 0
    in
    Meld.init m
        |> Meld.withTasks [ Api.msgpackBaseTypes ]
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
