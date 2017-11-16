module Meld
    exposing
        ( Meld
        , cmds
        , cmdseq
        , init
        , model
        , send
        , sequence
        , update
        , withCmds
        , withMerge
        , withTasks
        )

{-| Composeable `Task`s, instead of hundreds of Msg pattern match cases.

@docs Meld, cmds, cmdseq, init, model, send, sequence, update, withCmds, withMerge, withTasks

-}

import Task exposing (Task)


{-| Capture an application's model, tasks, tagged model merges and commands.
-}
type Meld m x msg
    = Meld
        { model : m
        , tasks : List (Meld m x msg -> Task x (Meld m x msg))
        , merges : List (m -> m)
        , commands : List (m -> Cmd msg)
        }


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


{-| Default initial/first `Task` for a sequnce of tasks.
-}
initTask : Meld m x msg -> Task x (Meld m x msg)
initTask meld =
    Task.succeed meld


{-| Get application's model.
-}
model : Meld m x msg -> m
model (Meld { model }) =
    model


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


{-| @private
Override `Meld m x msg` model.
-}
withModel : m -> Meld m x msg -> Meld m x msg
withModel m (Meld r) =
    Meld
        { r | model = m }


{-| Append a list of task functions to specified `Meld m x msg`.
-}
withTasks : List (Meld m x msg -> Task x (Meld m x msg)) -> Meld m x msg -> Meld m x msg
withTasks taskFns (Meld r) =
    Meld
        { r | tasks = r.tasks ++ taskFns }
