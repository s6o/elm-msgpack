module Api
    exposing
        ( msgpackBaseTypes
        )

import Hex
import Http
import HttpBuilder exposing (..)
import Json.Encode
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import MsgPack
import Task exposing (Task)


{-| @private
Configure HTTP GET request to test retrieving MessgePack's base types.
-}
requestMsgpackBaseTypes : Model -> Task Error String
requestMsgpackBaseTypes model =
    model.apiBase
        ++ "/msgpack-base-types"
        |> HttpBuilder.get
        |> withExpect Http.expectString
        |> withCacheBuster "_"
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp


{-| Process HTTP GET request response of MessagePack base types.
-}
msgpackBaseTypes : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
msgpackBaseTypes meld =
    requestMsgpackBaseTypes (Meld.model meld)
        |> Task.map
            (\binstr ->
                let
                    bytes =
                        MsgPack.asBytes binstr

                    msgpack =
                        MsgPack.decode bytes

                    model =
                        Meld.model meld

                    taskModel ma =
                        { ma | baseTypes = msgpack }

                    value =
                        case msgpack of
                            Err _ ->
                                Json.Encode.null

                            Ok mp ->
                                MsgPack.toJson mp |> Result.withDefault Json.Encode.null

                    l1 =
                        Debug.log "Decoded ints"
                            (bytes
                                |> List.map Hex.toString
                            )

                    l2 =
                        Debug.log "MsgPack Result" msgpack

                    l3 =
                        Debug.log "toJson" (Json.Encode.encode 0 value)
                in
                Meld.withMerge taskModel meld
            )
