module Api
    exposing
        ( msgpackBaseTypes
        )

import Hex
import Http
import HttpBuilder exposing (..)
import Json.Encode
import Meld exposing (Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import MsgPack as MP
import Task exposing (Task)


{-| @private
Configure HTTP GET request to test retrieving MessgePack's base types.
-}
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
                        MP.decode bytes

                    l1 =
                        Debug.log "Deocded ints"
                            (bytes
                                |> List.map Hex.toString
                            )

                    l2 =
                        Debug.log "MsgPack Result" msgpack

                    value =
                        case msgpack of
                            Err _ ->
                                Json.Encode.null

                            Ok mp ->
                                MP.toJson mp |> Result.withDefault Json.Encode.null

                    l3 =
                        Debug.log "toJson" (Json.Encode.encode 0 value)
                in
                meld
            )
