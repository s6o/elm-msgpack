module Messages
    exposing
        ( Msg(..)
        )

import Meld exposing (Error, Meld)
import Model exposing (Model)
import Task exposing (Task)


{-| Messages for Elm's runtime.

    * Acts - send (user) actions to executed
    * ActSeqs - send (user) actions to executed in sequence, one after the other
    * Requests - send HTTP API requests
    * Results - process the results of Acts or ActSeqs
    * Responses - process results of (HTTP) API requests

-}
type Msg
    = Acts (List (Meld Model Error Msg -> Task Error (Meld Model Error Msg)))
    | ActSeqs (List (Meld Model Error Msg -> Task Error (Meld Model Error Msg)))
    | Requests (List (Meld Model Error Msg -> Task Error (Meld Model Error Msg)))
    | Results Int (Result Error (Meld Model Error Msg))
    | Responses Int (Result Error (Meld Model Error Msg))
