module Model
    exposing
        ( Model
        , inAction
        , isLoading
        )

import MsgPack exposing (MsgPack)
import Result


type alias Model =
    { apiBase : String
    , actions : Int
    , loading : Int
    , errors : Maybe String
    , baseTypes : Result MsgPack.Error MsgPack
    }


inAction : Model -> Bool
inAction model =
    model.actions > 0


isLoading : Model -> Bool
isLoading model =
    model.loading > 0
