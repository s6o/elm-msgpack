module Messages
    exposing
        ( Msg(..)
        )

import Http
import Meld exposing (Meld)
import Model exposing (Model)


type Msg
    = Requests Int (Result Http.Error (Meld Model Http.Error Msg))
