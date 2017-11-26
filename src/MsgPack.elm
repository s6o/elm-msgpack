module MsgPack
    exposing
        ( Error(..)
        , Format
        , MsgPack(..)
        , MsgPackValue
        , asBytes
        , asString
        , isEmpty
        , isNil
        , pack
        , toBool
        , toBytes
        , toDict
        , toFloat
        , toInt
        , toList
        , toString
        , unpack
        )

{-| MessagePack for Elm.


# Specification

@docs Format, MsgPack, MsgPackValue, Error


# Serialization / Deserialization

@docs pack, unpack


# Conversions

@docs isEmpty, isNil, toBool, toBytes, toDict, toFloat, toInt, toList, toString


# HTTP


## Response

@docs asBytes


## Request

@docs asString

-}

import Array
import Bitwise
import Char
import Dict exposing (Dict)
import Result exposing (Result)
import String.UTF8 as Utf8


-- FORMAT


{-| MessagePack's type specific data layout specifications.
-}
type Format
    = Nil_ DataLayout
    | Array_ DataLayout
    | Bin_ DataLayout
    | Ext_ DataLayout
    | False_ DataLayout
    | FixArray_ DataLayout
    | FixExt_ DataLayout
    | FixMap_ DataLayout
    | FixNegInt_ DataLayout
    | FixPosInt_ DataLayout
    | FixStr_ DataLayout
    | Float_ DataLayout
    | Map_ DataLayout
    | Integer_ DataLayout
    | Str_ DataLayout
    | True_ DataLayout
    | Unsigned_ DataLayout


{-| @private
How is data length to be determined for a given `Format`.
-}
type DataLayout
    = Blocks Int Flag
    | BlocksType Int Int Flag
    | Bytes Int Flag
    | Fixed FlagMasked
    | None Flag
    | TypeBytes Int Int Flag


{-| @private
MessagePack format marker byte.
-}
type Flag
    = Flag Int


{-| @private
MessagePack format marker byte with a bit mask.
-}
type FlagMasked
    = FlagMasked Int Int


{-| @private
Parsed data bytes, format header has been removed from `bytes` for given `Format`.
-}
type alias Parsed =
    { format : Format
    , bytes : List Int
    , dataSize : Int
    }


{-| @private
Detect MessagePack `Format` from specified byte value.
-}
toFormat : Int -> Maybe Format
toFormat byte =
    case byte of
        0xC0 ->
            -- nil
            Just <| Nil_ <| None <| Flag byte

        0xC2 ->
            -- false
            Just <| False_ <| None <| Flag byte

        0xC3 ->
            -- true
            Just <| True_ <| None <| Flag byte

        0xC4 ->
            -- bin8
            Just <| Bin_ <| Blocks 1 <| Flag byte

        0xC5 ->
            -- bin16
            Just <| Bin_ <| Blocks 1 <| Flag byte

        0xC6 ->
            -- bin32
            Just <| Bin_ <| Blocks 1 <| Flag byte

        0xC7 ->
            -- ext8
            Just <| Ext_ <| BlocksType 1 1 <| Flag byte

        0xC8 ->
            -- ext16
            Just <| Ext_ <| BlocksType 2 1 <| Flag byte

        0xC9 ->
            -- ext32
            Just <| Ext_ <| BlocksType 4 1 <| Flag byte

        0xCA ->
            -- float32
            Just <| Float_ <| Bytes 4 <| Flag byte

        0xCB ->
            -- float64
            Just <| Float_ <| Bytes 8 <| Flag byte

        0xCC ->
            -- uint8
            Just <| Unsigned_ <| Bytes 1 <| Flag byte

        0xCD ->
            -- uint16
            Just <| Unsigned_ <| Bytes 2 <| Flag byte

        0xCE ->
            -- uint32
            Just <| Unsigned_ <| Bytes 4 <| Flag byte

        0xCF ->
            -- uint64
            Just <| Unsigned_ <| Bytes 8 <| Flag byte

        0xD0 ->
            -- int8
            Just <| Integer_ <| Bytes 1 <| Flag byte

        0xD1 ->
            -- int16
            Just <| Integer_ <| Bytes 2 <| Flag byte

        0xD2 ->
            -- int32
            Just <| Integer_ <| Bytes 4 <| Flag byte

        0xD3 ->
            Just <| Integer_ <| Bytes 8 <| Flag byte

        0xD4 ->
            -- fixext1
            Just <| FixExt_ <| TypeBytes 1 1 <| Flag byte

        0xD5 ->
            -- fixext2
            Just <| FixExt_ <| TypeBytes 1 2 <| Flag byte

        0xD6 ->
            -- fixext4
            Just <| FixExt_ <| TypeBytes 1 4 <| Flag byte

        0xD7 ->
            -- fixext8
            Just <| FixExt_ <| TypeBytes 1 8 <| Flag byte

        0xD8 ->
            -- fixext16
            Just <| FixExt_ <| TypeBytes 1 16 <| Flag byte

        0xD9 ->
            -- str8
            Just <| Str_ <| Blocks 1 <| Flag byte

        0xDA ->
            -- str16
            Just <| Str_ <| Blocks 2 <| Flag byte

        0xDB ->
            -- str32
            Just <| Str_ <| Blocks 4 <| Flag byte

        0xDC ->
            -- array16
            Just <| Array_ <| Blocks 2 <| Flag byte

        0xDD ->
            -- array32
            Just <| Array_ <| Blocks 4 <| Flag byte

        0xDE ->
            -- map16
            Just <| Map_ <| Blocks 2 <| Flag byte

        0xDF ->
            -- map32
            Just <| Map_ <| Blocks 4 <| Flag byte

        _ ->
            if byte >= 0x00 && byte <= 0x7F then
                -- pos fixint
                Just <| FixPosInt_ <| Fixed <| FlagMasked byte 0x7F
            else if byte >= 0x80 && byte <= 0x8F then
                -- fixmap
                Just <| Map_ <| Fixed <| FlagMasked byte 0x0F
            else if byte >= 0x90 && byte <= 0x9F then
                -- fixarray
                Just <| Array_ <| Fixed <| FlagMasked byte 0x0F
            else if byte >= 0xA0 && byte <= 0xBF then
                -- fixstr
                Just <| Str_ <| Fixed <| FlagMasked byte 0x1F
            else if byte >= 0xE0 && byte <= 0xFF then
                -- neg fixint
                Just <| FixNegInt_ <| Fixed <| FlagMasked byte 0x1F
            else
                Nothing


{-| @private
Parse chunk of data bytes accordingly to `Format`.
-}
parseFormat : Maybe Format -> List Int -> Result Error Parsed
parseFormat fmt bytes =
    fmt
        |> Maybe.map
            (\f ->
                let
                    d =
                        dataLayout f
                in
                case f of
                    FixNegInt_ (Fixed (FlagMasked byte mask)) ->
                        { format = f
                        , bytes = List.drop 1 bytes
                        , dataSize = byte
                        }

                    FixPosInt_ (Fixed (FlagMasked byte mask)) ->
                        { format = f
                        , bytes = List.drop 1 bytes
                        , dataSize = Bitwise.and byte mask
                        }

                    _ ->
                        { format = f
                        , bytes = List.drop (1 + blockCount d) bytes
                        , dataSize = dataLength d bytes
                        }
            )
        |> Result.fromMaybe UnknownFormat


{-| @private
Get data layout from `Format`.
-}
dataLayout : Format -> DataLayout
dataLayout fmt =
    case fmt of
        Nil_ d ->
            d

        Array_ d ->
            d

        Bin_ d ->
            d

        Ext_ d ->
            d

        False_ d ->
            d

        FixArray_ d ->
            d

        FixExt_ d ->
            d

        FixMap_ d ->
            d

        FixNegInt_ d ->
            d

        FixPosInt_ d ->
            d

        FixStr_ d ->
            d

        Float_ d ->
            d

        Map_ d ->
            d

        Integer_ d ->
            d

        Str_ d ->
            d

        True_ d ->
            d

        Unsigned_ d ->
            d


{-| @private
Number of data bytes accordingly `Format`'s data layout.
-}
dataLength : DataLayout -> List Int -> Int
dataLength data bytes =
    case data of
        Blocks c _ ->
            List.drop 1 bytes
                |> List.take c
                |> byteValue

        BlocksType c _ _ ->
            List.drop 1 bytes
                |> List.take c
                |> byteValue

        Bytes c _ ->
            c

        None _ ->
            0

        Fixed (FlagMasked byte mask) ->
            Bitwise.and byte mask

        TypeBytes t c _ ->
            t + c


{-| @private
Number of bytes used for block specifing data length.
-}
blockCount : DataLayout -> Int
blockCount data =
    case data of
        Blocks c _ ->
            c

        BlocksType c _ _ ->
            c

        _ ->
            0


{-| @private
Given a number of bytes in big-endian, construct an integer value.
-}
byteValue : List Int -> Int
byteValue bytes =
    bytes
        |> List.foldr
            (\b ( a, bi ) -> ( a + Bitwise.shiftLeftBy (bi * 8) b, bi + 1 ))
            ( 0, 0 )
        |> (\( a, _ ) -> a)



-- MessagePack


{-| Elm's MessagePack types.

To limit name conflicts some of MessagePack specification types are renamed:

    Array -> Vector
    Bin -> Blob
    Float -> Double
    String -> Text

-}
type MsgPack
    = Empty
    | Nil (MsgPackValue Never)
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
    , value : Maybe d
    }


{-| MessagePack processing errors.
-}
type Error
    = AppendFailure String
    | ConversionFailure String
    | UnknownFormat
    | ValueNotSet


{-| Serialize to list of bytes.
-}
pack : MsgPack -> List Int
pack msgpack =
    []


{-| Deserialize a list of bytes into `MsgPack`.

As with JSON, the MessagePack byte stream has to start with a container: Map or
Vector (array).

-}
unpack : List Int -> Result Error MsgPack
unpack bytes =
    let
        parse blist accum =
            case blist of
                [] ->
                    case List.head accum of
                        Nothing ->
                            Ok Empty

                        Just ( ct, _ ) ->
                            Ok ct

                b :: list ->
                    case
                        toFormat b
                            |> parseMsgPack (b :: list)
                    of
                        Err error ->
                            Err error

                        Ok ( mp, next ) ->
                            let
                                bytes =
                                    b :: list

                                nextAccum =
                                    case mp of
                                        Map { format } ->
                                            ( mp, 2 * dataLength (dataLayout format) bytes ) :: accum

                                        Vector { format } ->
                                            ( mp, dataLength (dataLayout format) bytes ) :: accum

                                        _ ->
                                            case accum of
                                                [] ->
                                                    accum

                                                ( collection, itemCount ) :: rest ->
                                                    let
                                                        newCollection =
                                                            append mp collection
                                                    in
                                                    case newCollection of
                                                        Err _ ->
                                                            ( collection, itemCount ) :: rest

                                                        Ok nc ->
                                                            if itemCount - 1 == 0 then
                                                                case rest of
                                                                    [] ->
                                                                        rest

                                                                    ( ct, ic ) :: accums ->
                                                                        case append nc ct of
                                                                            Err _ ->
                                                                                ( ct, ic ) :: accums

                                                                            Ok c ->
                                                                                ( c, ic - 1 ) :: accums
                                                            else
                                                                ( nc, itemCount - 1 ) :: rest
                            in
                            parse next nextAccum
    in
    parse bytes []


{-| Check if `MsgPack` result is empty.
-}
isEmpty : MsgPack -> Bool
isEmpty msgpack =
    case msgpack of
        Empty ->
            True

        _ ->
            False


{-| Check if `MsgPack` value is Nil.
-}
isNil : MsgPack -> Bool
isNil msgpack =
    case msgpack of
        Nil _ ->
            True

        _ ->
            False


{-| Convert from `MsgPack`'s Boolean to Elm's Bool.
-}
toBool : MsgPack -> Result Error Bool
toBool msgpack =
    case msgpack of
        Boolean { value } ->
            Maybe.map Ok value
                |> Maybe.withDefault (Err ValueNotSet)

        _ ->
            ("Expected MsgPack's Boolean, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Blob or Extension to a list of bytes.
In case of Extension the first byte in the list represents the 'type' byte.
-}
toBytes : MsgPack -> Result Error (List Int)
toBytes msgpack =
    case msgpack of
        Blob { value } ->
            Maybe.map Ok value
                |> Maybe.withDefault (Err ValueNotSet)

        Extension { value } ->
            Maybe.map (\( t, bytes ) -> Ok <| t :: bytes) value
                |> Maybe.withDefault (Err ValueNotSet)

        _ ->
            ("Expected MsgPack's Blob or Extension, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `Msgpack`'s Map to Elm's Dict.
-}
toDict : MsgPack -> Result Error (Dict String MsgPack)
toDict msgpack =
    case msgpack of
        Map { value } ->
            Maybe.map Ok value
                |> Maybe.withDefault (Err ValueNotSet)

        _ ->
            ("Expected MsgPack's Map, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Double or Text to Elm's `Float`.
-}
toFloat : MsgPack -> Result Error Float
toFloat msgpack =
    case msgpack of
        Double { value } ->
            Maybe.map Ok value
                |> Maybe.withDefault (Err ValueNotSet)

        Text { value } ->
            value
                |> Maybe.map
                    (\v ->
                        String.toFloat v
                            |> Result.mapError
                                (\x ->
                                    ("Conversion failure from MsgPack's Text to Float -" ++ x)
                                        |> ConversionFailure
                                )
                    )
                |> Maybe.withDefault (Err ValueNotSet)

        _ ->
            ("Expected MsgPack's Double, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Integer or Text to Elm's `Float`.
-}
toInt : MsgPack -> Result Error Int
toInt msgpack =
    case msgpack of
        Integer { value } ->
            Maybe.map Ok value
                |> Maybe.withDefault (Err ValueNotSet)

        Text { value } ->
            value
                |> Maybe.map
                    (\v ->
                        String.toInt v
                            |> Result.mapError
                                (\x ->
                                    ("Conversion failure from Text to Int -" ++ x)
                                        |> ConversionFailure
                                )
                    )
                |> Maybe.withDefault (Err ValueNotSet)

        _ ->
            ("Expected MsgPack's Integer, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Vector to Elm's List.
-}
toList : MsgPack -> Result Error (List MsgPack)
toList msgpack =
    case msgpack of
        Vector { value } ->
            Maybe.map Ok value
                |> Maybe.withDefault (Err ValueNotSet)

        _ ->
            ("Expected MsgPack's Vector, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Text to Elm's String.
-}
toString : MsgPack -> Result Error String
toString msgpack =
    case msgpack of
        Text { value } ->
            Maybe.map Ok value
                |> Maybe.withDefault (Err ValueNotSet)

        _ ->
            ("Expected MsgPack's Text, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| @private
-}
parseMsgPack : List Int -> Maybe Format -> Result Error ( MsgPack, List Int )
parseMsgPack bytes fmt =
    parseFormat fmt bytes
        |> Result.map
            (\r ->
                case r.format of
                    Nil_ _ ->
                        ( Nil { format = r.format, value = Nothing }
                        , r.bytes
                        )

                    Array_ _ ->
                        parseArray r

                    Bin_ _ ->
                        ( Blob
                            { format = r.format
                            , value = Just <| List.take r.dataSize r.bytes
                            }
                        , List.drop r.dataSize r.bytes
                        )

                    Ext_ _ ->
                        parseExtension r

                    False_ _ ->
                        parseBoolean r False

                    FixArray_ _ ->
                        parseArray r

                    FixExt_ _ ->
                        parseExtension r

                    FixMap_ _ ->
                        parseMap r

                    FixNegInt_ _ ->
                        ( Integer
                            { format = r.format
                            , value =
                                Bitwise.and r.dataSize 0xFF
                                    |> Bitwise.or 0x000FFFFFFFFFFF00
                                    |> Just
                            }
                        , r.bytes
                        )

                    FixPosInt_ _ ->
                        ( Integer
                            { format = r.format
                            , value = Just r.dataSize
                            }
                        , r.bytes
                        )

                    FixStr_ _ ->
                        parseText r

                    Float_ _ ->
                        if r.dataSize == 4 then
                            parseFloat32 r
                        else
                            parseFloat64 r

                    Map_ _ ->
                        parseMap r

                    Integer_ _ ->
                        parseInteger r

                    Str_ _ ->
                        parseText r

                    True_ _ ->
                        parseBoolean r True

                    Unsigned_ _ ->
                        parseUnsigned r
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
                    | value =
                        Maybe.map
                            (\d ->
                                case Dict.get appendKey d of
                                    Nothing ->
                                        Dict.insert appendKey item d

                                    Just kv ->
                                        Dict.insert (asKey kv) item d
                                            |> Dict.remove appendKey
                            )
                            r.value
                }
                |> Ok

        Vector r ->
            Vector { r | value = r.value |> Maybe.map (\l -> l ++ [ item ]) }
                |> Ok

        _ ->
            "2nd argument needs to be a MsgPack collection type: Map or Vector."
                |> AppendFailure
                |> Err


{-| @private
Temporary `MsgPack`'s Map key used to collect actual key value, before processing key's item.
-}
appendKey : String
appendKey =
    "__elm-msgpack-key__"


{-| @private
Convert `MsgPack`'s `MsgPackValue` type's `value` member value to `String`.
-}
asKey : MsgPack -> String
asKey msgpack =
    case msgpack of
        Empty ->
            "__elm-msgpack-empty__"

        Nil _ ->
            "__elm-msgpack-nil__"

        Blob { value } ->
            value
                |> Maybe.map (\bytes -> Utf8.toString bytes |> Result.withDefault "__elm-msgpack-bin__")
                |> Maybe.withDefault "__elm-msgpack-bin__"

        Boolean { value } ->
            value
                |> Maybe.map (\b -> "__elm-msgpack-bool-" ++ Basics.toString b ++ "__")
                |> Maybe.withDefault "__elm-msgpack-bool__"

        Extension { value } ->
            value
                |> Maybe.map
                    (\( _, b ) ->
                        Utf8.toString b
                            |> Result.withDefault "__elm-msgpack-ext__"
                    )
                |> Maybe.withDefault "__elm-msgpack-ext__"

        Double { value } ->
            value
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-double__"

        Integer { value } ->
            value
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-integer__"

        Map { value } ->
            value
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-map__"

        Text { value } ->
            value
                |> Maybe.map identity
                |> Maybe.withDefault "__elm-msgpack-text__"

        Vector { value } ->
            value
                |> Maybe.map Basics.toString
                |> Maybe.withDefault "__elm-msgpack-vector__"


{-| @private
-}
parseArray : Parsed -> ( MsgPack, List Int )
parseArray r =
    ( Vector { format = r.format, value = Just [] }
    , r.bytes
    )


{-| @private
-}
parseBoolean : Parsed -> Bool -> ( MsgPack, List Int )
parseBoolean r flag =
    ( Boolean { format = r.format, value = Just flag }
    , r.bytes
    )


{-| @private
-}
parseExtension : Parsed -> ( MsgPack, List Int )
parseExtension r =
    ( Extension
        { format = r.format
        , value =
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


{-| @private
-}
parseFloat32 : Parsed -> ( MsgPack, List Int )
parseFloat32 r =
    let
        rawBits =
            List.take r.dataSize r.bytes
                |> byteValue

        sign =
            Bitwise.shiftLeftBy 31 1
                |> Bitwise.and rawBits
                |> (\b -> -1 ^ b)
                |> Basics.toFloat

        exp =
            (Bitwise.shiftRightZfBy 23 rawBits |> Bitwise.and 0xFF)
                - 127
                |> Basics.toFloat

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
                            fs + Basics.toFloat (2 ^ (-1 * i))
                        else
                            fs
                    )
                    1.0
    in
    ( Double
        { format = r.format
        , value = Just <| sign * (fsum * (2 ^ exp))
        }
    , List.drop r.dataSize r.bytes
    )


{-| @private
-}
parseFloat64 : Parsed -> ( MsgPack, List Int )
parseFloat64 r =
    let
        rawBits =
            List.take r.dataSize r.bytes
                |> byteValue

        sign =
            Bitwise.shiftLeftBy 63 1
                |> Bitwise.and rawBits
                |> (\b -> -1 ^ b)
                |> Basics.toFloat

        exp =
            (Bitwise.shiftRightZfBy 51 rawBits |> Bitwise.and 0x07FF)
                - 1023
                |> Basics.toFloat

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
                            fs + Basics.toFloat (2 ^ (-1 * i))
                        else
                            fs
                    )
                    1.0
    in
    ( Double
        { format = r.format
        , value = Just <| sign * (fsum * (2 ^ exp))
        }
    , List.drop r.dataSize r.bytes
    )


{-| @private
-}
parseInteger : Parsed -> ( MsgPack, List Int )
parseInteger r =
    let
        bytes =
            List.take r.dataSize r.bytes

        mask =
            List.foldr (\b ( a, bi ) -> ( a + Bitwise.shiftLeftBy (bi * 8) 0xFF, bi + 1 )) ( 0, 0 ) bytes
                |> (\( a, _ ) -> Bitwise.xor a 0x000FFFFFFFFFFFFF)

        value =
            bytes
                |> List.foldr
                    (\b ( a, bi ) ->
                        ( a + (Bitwise.and b 0xFF |> Bitwise.shiftLeftBy (bi * 8)), bi + 1 )
                    )
                    ( 0, 0 )
                |> (\( a, _ ) -> Bitwise.or a mask)
    in
    ( Integer
        { format = r.format
        , value =
            Just <| value
        }
    , List.drop r.dataSize r.bytes
    )


{-| @private
-}
parseMap : Parsed -> ( MsgPack, List Int )
parseMap r =
    ( Map { format = r.format, value = Just <| Dict.empty }
    , r.bytes
    )


{-| @private
-}
parseText : Parsed -> ( MsgPack, List Int )
parseText r =
    ( Text
        { format = r.format
        , value =
            List.take r.dataSize r.bytes
                |> Utf8.toString
                |> Result.withDefault ""
                |> Just
        }
    , List.drop r.dataSize r.bytes
    )


{-| @private
-}
parseUnsigned : Parsed -> ( MsgPack, List Int )
parseUnsigned r =
    ( Integer { format = r.format, value = Just <| byteValue <| List.take r.dataSize r.bytes }
    , List.drop r.dataSize r.bytes
    )



-- HTTP


{-| Convert to bytes from a string representation delivered as 'text/plain; charset=x-user-defined'.
-}
asBytes : String -> List Int
asBytes bstr =
    String.toList bstr
        |> List.map (Char.toCode >> Bitwise.and 0xFF)


{-| Convert from bytes to a string representation to be sent with the HTTP
Content-Type header set to 'text/plain; charset=x-user-defined'.
-}
asString : List Int -> String
asString bstr =
    "TODO"
