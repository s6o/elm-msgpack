module Meld
    exposing
        ( Error(..)
        , Meld
        , addTasks
        , cmds
        , cmdseq
        , errorMessage
        , httpError
        , init
        , model
        , send
        , sequence
        , update
        , withCmds
        , withMerge
        )

{-| Composeable `Task`s, instead of hundreds of Msg pattern match cases.

    * https://github.com/s6o/elm-meld

@docs Meld, Error


# Task mapping

@docs model, withCmds, withMerge


# Task preparation

Set up tasks with a tag (Msg) for Elm's runtime to be executed.

@docs init, addTasks, send, sequence


# Command preparation

@docs cmds, cmdseq


# Task result processing

@docs update, errorMessage, httpError

-}

import Http
import Task exposing (Task)


{-| Capture an application's model and tasks, process results with model merges and commands.
-}
type Meld m x msg
    = Meld
        { model : m
        , tasks : List (Meld m x msg -> Task x (Meld m x msg))
        , merges : List (m -> m)
        , commands : List (m -> Cmd msg)
        }


{-| Common error handling type to ease composability.
-}
type Error
    = EMsg String
    | EHttp Http.Error



-- Task mapping


{-| Get application's model.
-}
model : Meld m x msg -> m
model (Meld { model }) =
    model


{-| Append a list of command functions to specified `Meld m x msg`.
-}
withCmds : List (m -> Cmd msg) -> Meld m x msg -> Meld m x msg
withCmds cmdFns (Meld r) =
    Meld
        { r | commands = r.commands ++ cmdFns }


{-| Apply and append a merge function to specified `Meld m x msg`.
-}
withMerge : (m -> m) -> Meld m x msg -> Meld m x msg
withMerge mergeFn (Meld r) =
    Meld
        { r
            | model = mergeFn r.model
            , merges = mergeFn :: r.merges
        }



-- Task preparation


{-| Create an initial `Meld m x msg` from specified model.
-}
init : m -> Meld m x msg
init m =
    Meld
        { model = m
        , tasks = []
        , merges = []
        , commands = []
        }


{-| @private
Default initial/first `Task` for a sequnce of tasks.
-}
initTask : Meld m x msg -> Task x (Meld m x msg)
initTask meld =
    Task.succeed meld


{-| Append a list of task functions to specified `Meld m x msg`.
-}
addTasks : List (Meld m x msg -> Task x (Meld m x msg)) -> Meld m x msg -> Meld m x msg
addTasks taskFns (Meld r) =
    Meld
        { r | tasks = r.tasks ++ taskFns }


{-| Update model task count and create `Cmd`s from `Meld m x msg` tasks to be
executed in any/unspecified order.
-}
send : (Int -> Result x (Meld m x msg) -> msg) -> (m -> Int) -> (Int -> m) -> Meld m x msg -> ( m, Cmd msg )
send toMsg taskCountFn storeCountFn (Meld r) =
    if List.isEmpty r.tasks then
        ( r.model
        , Cmd.none
        )
    else
        let
            nextModel =
                taskCountFn r.model
                    |> (\tc -> tc + List.length r.tasks)
                    |> storeCountFn
        in
        ( nextModel
        , withModel nextModel (Meld r)
            |> cmds (toMsg 1)
        )


{-| Update model task count and execute `Meld m x msg` tasks in sequence,
by continueing to a next task only upon successful completion of the previous task.
-}
sequence : (Int -> Result x (Meld m x msg) -> msg) -> (m -> Int) -> (Int -> m) -> Meld m x msg -> ( m, Cmd msg )
sequence toMsg taskCountFn storeCountFn (Meld r) =
    if List.isEmpty r.tasks then
        ( r.model
        , Cmd.none
        )
    else
        let
            taskCount =
                List.length r.tasks

            nextModel =
                taskCountFn r.model
                    |> (\tc -> tc + taskCount)
                    |> storeCountFn
        in
        ( nextModel
        , withModel nextModel (Meld r)
            |> cmdseq (toMsg taskCount)
        )


{-| @private
Override `Meld m x msg` model.
-}
withModel : m -> Meld m x msg -> Meld m x msg
withModel m (Meld r) =
    Meld
        { r | model = m }



-- CMD management


{-| Execute a set of `Meld m x msg`'s tasks in any/unspecified order.
-}
cmds : (Result x (Meld m x msg) -> msg) -> Meld m x msg -> Cmd msg
cmds toMsg meld =
    let
        (Meld { tasks }) =
            meld
    in
    tasks
        |> List.map (\tf -> tf meld |> Task.attempt toMsg)
        |> Cmd.batch


{-| Execute a set of `Meld m x msg`'s tasks in sequence, by proceeding to the next
only upon successful execution.
-}
cmdseq : (Result x (Meld m x msg) -> msg) -> Meld m x msg -> Cmd msg
cmdseq toMsg meld =
    let
        (Meld { tasks }) =
            meld
    in
    tasks
        |> List.foldl Task.andThen (initTask meld)
        |> Task.attempt toMsg



-- Task result processing


{-| Apply model merges to `m`, update active task count and create command batch.
-}
update : Int -> (m -> Int) -> (Int -> m) -> m -> Meld m x msg -> ( m, Cmd msg )
update taskCount modelCountFn storeCountFn appModel (Meld { merges, commands }) =
    let
        finalModel =
            merges
                |> List.foldr
                    (\mergeFn accumModel -> mergeFn accumModel)
                    (modelCountFn appModel - taskCount |> storeCountFn)
    in
    ( finalModel
    , commands
        |> List.map (\cmdFn -> cmdFn finalModel)
        |> Cmd.batch
    )


{-| Retrive the error message.
-}
errorMessage : Error -> String
errorMessage error =
    case error of
        EMsg msg ->
            msg

        EHttp httpError ->
            case httpError of
                Http.BadUrl msg ->
                    msg

                Http.Timeout ->
                    "Request timeout."

                Http.NetworkError ->
                    "Network error."

                Http.BadStatus { status } ->
                    toString status.code ++ " | " ++ status.message

                Http.BadPayload dbg { status } ->
                    toString status.code ++ " | " ++ status.message ++ " | " ++ dbg


{-| Helper to access Http.Error is ther is one
-}
httpError : Error -> Maybe Http.Error
httpError error =
    case error of
        EHttp httpError ->
            Just httpError

        _ ->
            Nothing
