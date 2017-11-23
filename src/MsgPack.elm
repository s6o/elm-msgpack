module MsgPack
    exposing
        ( MsgPack(..)
        , MsgPackValue
        , asBytes
        , fromMsgPack
        , toMsgPack
        , toString
        )

{-| MsgPack byte stream handling.


# HTTP


## Response

@docs asBytes


## Request

@docs toString


# Serialization / Deserialization

@docs MsgPack, MsgPackValue, fromMsgPack, toMsgPack

-}

import Array
import Bitwise
import Char
import Dict exposing (Dict)
import MsgPack.Error as Error exposing (Error)
import MsgPack.Format as Fmt exposing (Format(..), Parsed)
import Result exposing (Result)
import String.UTF8 as Utf8


{-| Convert to bytes from a string representation delivered as 'text/plain; charset=x-user-defined'.
-}
asBytes : String -> List Int
asBytes bstr =
    String.toList bstr
        |> List.map (Char.toCode >> Bitwise.and 0xFF)


{-| Convert from bytes to string representation to be sent with the HTTP
Content-Type header as 'text/plain; charset=x-user-defined'.
-}
toString : List Int -> String
toString bstr =
    "TODO"


{-| Elm's MessagePack byte-stream wrapper.

To limit name conflicts some of MessagePack specification types are renamed:

    Array -> Vector
    Bin -> Blob
    Float -> Double
    String -> Text

-}
type MsgPack
    = Nil (MsgPackValue Never)
    | Blob (MsgPackValue (List Int))
    | Boolean (MsgPackValue Bool)
    | Extension (MsgPackValue ( Int, List Int ))
    | Double (MsgPackValue Float)
    | Integer (MsgPackValue Int)
    | Map (MsgPackValue (Dict String MsgPack))
    | Text (MsgPackValue String)
    | Vector (MsgPackValue (List MsgPack))


{-| Generic `MsgPack` value container.
-}
type alias MsgPackValue d =
    { format : Format
    , data : Maybe d
    }


{-| Serialize to list of bytes.
-}
fromMsgPack : MsgPack -> List Int
fromMsgPack msgpack =
    []


{-| Deserialize a list of bytes into `MsgPack`.

As with JSON, the MessagePack byte stream has to start with a container: Map or
Vector (array).

-}
toMsgPack : List Int -> Result Error MsgPack
toMsgPack bytes =
    let
        parse blist accum =
            case blist of
                [] ->
                    Result.fromMaybe Error.EmptyStream accum

                b :: list ->
                    case
                        Fmt.format b
                            |> parseFormat ( accum, b :: list )
                    of
                        Err error ->
                            Err error

                        Ok ( mp, next ) ->
                            let
                                nextAccum =
                                    Maybe.map (append mp) accum
                                        |> Maybe.withDefault (Ok mp)
                                        |> Result.toMaybe
                            in
                            parse next nextAccum
    in
    parse bytes Nothing


{-| @private
-}
parseFormat : ( Maybe MsgPack, List Int ) -> Maybe Format -> Result Error ( MsgPack, List Int )
parseFormat ( msgpack, bytes ) fmt =
    Fmt.parse fmt bytes
        |> Result.map
            (\r ->
                case r.format of
                    Fmt.Nil _ ->
                        ( Nil { format = r.format, data = Nothing }
                        , r.bytes
                        )

                    Fmt.Array_ _ ->
                        parseArray r

                    Fmt.Bin _ ->
                        ( Blob
                            { format = r.format
                            , data = Just <| List.take r.dataSize r.bytes
                            }
                        , List.drop r.dataSize r.bytes
                        )

                    Fmt.Ext _ ->
                        parseExtension r

                    Fmt.False_ _ ->
                        parseBoolean r False

                    Fmt.FixArray _ ->
                        parseArray r

                    Fmt.FixExt _ ->
                        parseExtension r

                    Fmt.FixMap _ ->
                        parseMap r

                    Fmt.FixStr _ ->
                        parseText r

                    Fmt.Float_ _ ->
                        if r.dataSize == 4 then
                            parseFloat32 r
                        else
                            parseFloat64 r

                    Fmt.Map _ ->
                        parseMap r

                    Fmt.Integer _ ->
                        parseInteger r

                    Fmt.Str _ ->
                        parseText r

                    Fmt.True_ _ ->
                        parseBoolean r True

                    Fmt.Unsigned _ ->
                        parseInteger r
            )


{-| @private
Append a `MsgPack` item to `MsgPack` collection (array or map).
-}
append : MsgPack -> MsgPack -> Result Error MsgPack
append item collection =
    case collection of
        Map r ->
            Map
                { r
                    | data =
                        Maybe.map
                            (\d ->
                                case Dict.get appendKey d of
                                    Nothing ->
                                        Dict.insert appendKey item d

                                    Just kv ->
                                        Dict.insert (asKey kv) item d
                                            |> Dict.remove appendKey
                            )
                            r.data
                }
                |> Ok

        Vector r ->
            Vector { r | data = r.data |> Maybe.map (\l -> l ++ [ item ]) }
                |> Ok

        _ ->
            "2nd argument needs to be a MsgPack collection type: Map or Vector."
                |> Error.AppendFailure
                |> Err


{-| @private
Temporary `MsgPack`'s Map key used to collect actual key value, before processing key's item.
-}
appendKey : String
appendKey =
    "__elm-msgpack-key__"


{-| @private
Convert `MsgPack`'s `MsgPackValue` type's `data` member value to `String`.
-}
asKey : MsgPack -> String
asKey msgpack =
    case msgpack of
        Nil _ ->
            "__elm-msgpack-nil__"

        Blob { data } ->
            data
                |> Maybe.map (\bytes -> Utf8.toString bytes |> Result.withDefault "__elm-msgpack-bin__")
                |> Maybe.withDefault "__elm-msgpack-bin__"

        Boolean { data } ->
            data
                |> Maybe.map (\b -> "__elm-msgpack-bool-" ++ Basics.toString b ++ "__")
                |> Maybe.withDefault "__elm-msgpack-bool__"

        Extension { data } ->
            data
                |> Maybe.map
                    (\( _, b ) ->
                        Utf8.toString b
                            |> Result.withDefault "__elm-msgpack-ext__"
                    )
                |> Maybe.withDefault "__elm-msgpack-ext__"

        Double { data } ->
            data
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-double__"

        Integer { data } ->
            data
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-integer__"

        Map { data } ->
            data
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-map__"

        Text { data } ->
            data
                |> Maybe.map identity
                |> Maybe.withDefault "__elm-msgpack-text__"

        Vector { data } ->
            data
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-vector__"


parseArray : Parsed -> ( MsgPack, List Int )
parseArray r =
    ( Vector { format = r.format, data = Just [] }
    , r.bytes
    )


parseBoolean : Parsed -> Bool -> ( MsgPack, List Int )
parseBoolean r flag =
    ( Boolean { format = r.format, data = Just flag }
    , r.bytes
    )


parseExtension : Parsed -> ( MsgPack, List Int )
parseExtension r =
    ( Extension
        { format = r.format
        , data =
            Just
                ( List.take 1 r.bytes
                    |> List.head
                    |> Maybe.withDefault 0
                , List.drop 1 r.bytes
                    |> List.take (r.dataSize - 1)
                )
        }
    , List.drop r.dataSize r.bytes
    )


parseFloat32 : Parsed -> ( MsgPack, List Int )
parseFloat32 r =
    let
        rawBits =
            List.take r.dataSize r.bytes
                |> Fmt.byteValue

        sign =
            Bitwise.shiftLeftBy 31 1
                |> Bitwise.and rawBits
                |> (\b -> -1 ^ b)
                |> toFloat

        exp =
            (Bitwise.shiftRightZfBy 23 rawBits |> Bitwise.and 0xFF)
                - 127
                |> toFloat

        frac =
            Bitwise.and rawBits 0x007FFFFF

        fsum =
            Array.initialize 23 (\n -> 22 - n)
                |> Array.toList
                |> List.foldl
                    (\i fs ->
                        let
                            bit =
                                Bitwise.shiftLeftBy i 1
                                    |> Bitwise.and frac
                        in
                        if bit == 1 then
                            fs + toFloat (2 ^ (-1 * i))
                        else
                            fs
                    )
                    1.0
    in
    ( Double
        { format = r.format
        , data = Just <| sign * (fsum * (2 ^ exp))
        }
    , List.drop r.dataSize r.bytes
    )


parseFloat64 : Parsed -> ( MsgPack, List Int )
parseFloat64 r =
    let
        rawBits =
            List.take r.dataSize r.bytes
                |> Fmt.byteValue

        sign =
            Bitwise.shiftLeftBy 63 1
                |> Bitwise.and rawBits
                |> (\b -> -1 ^ b)
                |> toFloat

        exp =
            (Bitwise.shiftRightZfBy 51 rawBits |> Bitwise.and 0x07FF)
                - 1023
                |> toFloat

        frac =
            Bitwise.and rawBits 0x000FFFFFFFFFFFFF

        fsum =
            Array.initialize 52 (\n -> 51 - n)
                |> Array.toList
                |> List.foldl
                    (\i fs ->
                        let
                            bit =
                                Bitwise.shiftLeftBy i 1
                                    |> Bitwise.and frac
                        in
                        if bit == 1 then
                            fs + toFloat (2 ^ (-1 * i))
                        else
                            fs
                    )
                    1.0
    in
    ( Double
        { format = r.format
        , data = Just <| sign * (fsum * (2 ^ exp))
        }
    , List.drop r.dataSize r.bytes
    )


parseInteger : Parsed -> ( MsgPack, List Int )
parseInteger r =
    ( Integer { format = r.format, data = Just <| Fmt.byteValue <| List.take r.dataSize r.bytes }
    , List.drop r.dataSize r.bytes
    )


parseMap : Parsed -> ( MsgPack, List Int )
parseMap r =
    ( Map { format = r.format, data = Just <| Dict.empty }
    , r.bytes
    )


parseText : Parsed -> ( MsgPack, List Int )
parseText r =
    ( Text
        { format = r.format
        , data =
            List.take r.dataSize r.bytes
                |> Utf8.toString
                |> Result.withDefault ""
                |> Just
        }
    , List.drop r.dataSize r.bytes
    )
