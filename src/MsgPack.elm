module MsgPack
    exposing
        ( Error(..)
        , MsgPack
        , asBytes
        , asString
        , bin
        , bool
        , decode
        , empty
        , encode
        , ext
        , float
        , int
        , isEmpty
        , isNil
        , list
        , nil
        , object
        , string
        , toBool
        , toBytes
        , toDict
        , toFloat
        , toInt
        , toJson
        , toList
        , toString
        )

{-| MessagePack for Elm.


# Specification

@docs MsgPack, Error


# Decoding

@docs decode


## Conversions

@docs isEmpty, isNil, toBool, toBytes, toDict, toFloat, toInt, toJson, toList, toString


# Encoding

@docs encode


## Primitivies

@docs empty, nil, bin, bool, float, ext, int, string


## Containers

@docs list, object


# HTTP


## Response

@docs asBytes


## Request

@docs asString

-}

import Bitwise
import Char
import Dict exposing (Dict)
import Json.Encode exposing (Value)
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
            Just <| Bin_ <| Blocks 2 <| Flag byte

        0xC6 ->
            -- bin32
            Just <| Bin_ <| Blocks 4 <| Flag byte

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
                |> (\bv -> bv + 1)

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


{-| @private
Given an float value split it into bytes in little-endian order.
-}
floatBytes : Float -> List Int
floatBytes value =
    let
        fstr =
            Basics.toString value

        parts =
            String.split "." fstr

        parseIntBits num accum =
            case num of
                0 ->
                    accum

                _ ->
                    parseIntBits (num // 2) (rem num 2 :: accum)

        intBits =
            List.head parts
                |> Maybe.map
                    (\s ->
                        case String.toInt s of
                            Err _ ->
                                ""

                            Ok i ->
                                parseIntBits (i // 2) (rem i 2 :: [])
                                    |> List.map Basics.toString
                                    |> String.join ""
                    )
                |> Maybe.withDefault ""

        fracBits =
            List.drop 1 parts
                |> List.head
                |> Maybe.map
                    (\s ->
                        case String.toFloat ("0." ++ s) of
                            Err _ ->
                                ""

                            Ok f ->
                                let
                                    parseFracBits fnum idx accum =
                                        case idx > 52 || fnum >= 1 of
                                            True ->
                                                accum

                                            False ->
                                                parseFracBits
                                                    (if (fnum * 2) > 1 then
                                                        (fnum * 2) - (Basics.toFloat <| Basics.floor (fnum * 2))
                                                     else
                                                        fnum * 2
                                                    )
                                                    (idx + 1)
                                                    (Basics.floor (fnum * 2) :: accum)
                                in
                                parseFracBits f 0 []
                                    |> List.map Basics.toString
                                    |> List.reverse
                                    |> String.join ""
                                    |> String.padRight 52 '0'
                    )
                |> Maybe.withDefault ""

        ( exp, mantissa ) =
            if String.startsWith "1" intBits then
                ( String.length intBits - 1
                , (String.dropLeft 1 intBits ++ fracBits)
                    |> String.left 52
                )
            else
                fracBits
                    |> (\bits ->
                            let
                                scanToOne ch remaining pos =
                                    case ch of
                                        "1" ->
                                            ( pos, remaining )

                                        _ ->
                                            scanToOne
                                                (String.left 1 remaining)
                                                (String.dropLeft 1 remaining)
                                                (pos + 1)
                            in
                            scanToOne (String.left 1 bits) (String.dropLeft 1 bits) 1
                       )
                    |> (\( pos, remaining ) -> ( -1 * pos, String.left 52 remaining |> String.padRight 52 '0' ))

        expBits =
            (exp + 1023)
                |> (\n -> parseIntBits (n // 2) (rem n 2 :: []))
                |> List.map Basics.toString
                |> String.join ""
                |> String.padLeft 11 '0'

        signBit =
            if value >= 0 then
                "0"
            else
                "1"
    in
    (signBit ++ expBits ++ mantissa)
        |> (\bits ->
                let
                    nibbleMap =
                        [ ( "0000", 0x00 )
                        , ( "0001", 0x01 )
                        , ( "0010", 0x02 )
                        , ( "0011", 0x03 )
                        , ( "0100", 0x04 )
                        , ( "0101", 0x05 )
                        , ( "0110", 0x06 )
                        , ( "0111", 0x07 )
                        , ( "1000", 0x08 )
                        , ( "1001", 0x09 )
                        , ( "1010", 0x0A )
                        , ( "1011", 0x0B )
                        , ( "1100", 0x0C )
                        , ( "1101", 0x0D )
                        , ( "1110", 0x0E )
                        , ( "1111", 0x0F )
                        ]
                            |> Dict.fromList

                    splitToNibbles nibble remaining accum =
                        case nibble of
                            "" ->
                                accum

                            _ ->
                                Dict.get nibble nibbleMap
                                    |> Maybe.map (\ni -> ni :: accum)
                                    |> Maybe.withDefault accum
                                    |> splitToNibbles (String.left 4 remaining) (String.dropLeft 4 remaining)
                in
                splitToNibbles (String.left 4 bits) (String.dropLeft 4 bits) []
           )
        |> List.foldr
            (\n ( p, bytes ) ->
                case p of
                    Nothing ->
                        ( Just n, bytes )

                    Just hi ->
                        ( Nothing
                        , (Bitwise.shiftLeftBy 4 hi |> Bitwise.or n) :: bytes
                        )
            )
            ( Nothing, [] )
        |> (\( _, bytes ) -> bytes)


{-| @private
Given an int value split it into specified number of bytes in little-endian order.
-}
intBytes : Int -> Int -> List Int
intBytes value byteCount =
    if byteCount > 1 then
        List.repeat byteCount 0xFF
            |> List.foldl
                (\mask ( a, i ) ->
                    ( (Bitwise.shiftLeftBy i mask
                        |> Bitwise.and value
                        |> Bitwise.shiftRightZfBy i
                      )
                        :: a
                    , i - 8
                    )
                )
                ( [], (byteCount - 1) * 8 )
            |> (\( a, _ ) -> a)
    else
        []



-- MessagePack


{-| Elm's MessagePack types.

        MessagePack     Elm

        Nil                             -- helpers: isNil, nil
        Array           List MsgPack    -- note: in Elm 0.18 bigger `Array`s have issues
        Binary          List Int        -- list of byte values
        Boolean         Bool
        Extension       (Int, List Int) -- byte and list of bytes tuple
        Float           Float
        Integer         Int
        Map             Dict String MsgPack
        String          String

-}
type MsgPack
    = Empty
    | Nil
    | Bin (List Int)
    | Boolean Bool
    | Extension ( Int, List Int )
    | Double Float
    | Integer Int
    | Map Int (Dict String MsgPack)
    | Text String
    | Vector Int (List MsgPack)


{-| MessagePack processing errors.
-}
type Error
    = AppendFailure String
    | ConversionFailure String
    | UnknownFormat


{-| Decode a list of bytes into `MsgPack`.

As with JSON, the MessagePack byte stream has to start with a container: object or
array.

-}
decode : List Int -> Result Error MsgPack
decode bytes =
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
                                nextAccum =
                                    case mp of
                                        Map itemCount _ ->
                                            ( mp, 2 * itemCount ) :: accum

                                        Vector itemCount _ ->
                                            ( mp, itemCount ) :: accum

                                        _ ->
                                            unpackItem mp accum
                            in
                            parse next nextAccum

        unpackItem mp accum =
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
                                        ( nc, 0 ) :: []

                                    ( ct, ic ) :: accums ->
                                        case append nc ct of
                                            Err _ ->
                                                ( ct, ic ) :: accums

                                            Ok c ->
                                                ( c, ic - 1 ) :: accums
                            else
                                ( nc, itemCount - 1 ) :: rest
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
        Nil ->
            True

        _ ->
            False


{-| Convert from `MsgPack`'s Boolean to Elm's Bool.
-}
toBool : MsgPack -> Result Error Bool
toBool msgpack =
    case msgpack of
        Boolean value ->
            Ok value

        _ ->
            ("Expected MsgPack's Boolean, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Bin or Extension to a list of bytes.
In case of Extension the first byte in the list represents the 'type' byte.
-}
toBytes : MsgPack -> Result Error (List Int)
toBytes msgpack =
    case msgpack of
        Bin value ->
            Ok value

        Extension ( t, bytes ) ->
            Ok <| (t :: bytes)

        _ ->
            ("Expected MsgPack's Bin or Extension, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `Msgpack`'s Map to Elm's Dict.
-}
toDict : MsgPack -> Result Error (Dict String MsgPack)
toDict msgpack =
    case msgpack of
        Map _ value ->
            Ok value

        _ ->
            ("Expected MsgPack's Map, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Double or Text to Elm's `Float`.
-}
toFloat : MsgPack -> Result Error Float
toFloat msgpack =
    case msgpack of
        Double value ->
            Ok value

        Text value ->
            String.toFloat value
                |> Result.mapError
                    (\x ->
                        ("Conversion failure from MsgPack's Text to Float -" ++ x)
                            |> ConversionFailure
                    )

        _ ->
            ("Expected MsgPack's Double, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Integer or Text to Elm's `Float`.
-}
toInt : MsgPack -> Result Error Int
toInt msgpack =
    case msgpack of
        Integer value ->
            Ok value

        Text value ->
            String.toInt value
                |> Result.mapError
                    (\x ->
                        ("Conversion failure from Text to Int -" ++ x)
                            |> ConversionFailure
                    )

        _ ->
            ("Expected MsgPack's Integer, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack` to `Json.Encode.Value`.
-}
toJson : MsgPack -> Result Error Value
toJson msgpack =
    let
        asJson mp =
            case mp of
                Empty ->
                    Json.Encode.null

                Nil ->
                    Json.Encode.null

                Bin value ->
                    (List.map Json.Encode.int >> Json.Encode.list) value

                Boolean value ->
                    Json.Encode.bool value

                Extension ( t, bytes ) ->
                    (t :: bytes)
                        |> List.map Json.Encode.int
                        |> Json.Encode.list

                Double value ->
                    Json.Encode.float value

                Integer value ->
                    Json.Encode.int value

                Map _ value ->
                    Dict.map (\_ v -> asJson v) value
                        |> Dict.toList
                        |> Json.Encode.object

                Text value ->
                    Json.Encode.string value

                Vector _ value ->
                    (List.map asJson >> Json.Encode.list) value
    in
    case msgpack of
        Map _ _ ->
            asJson msgpack
                |> Ok

        Vector _ _ ->
            asJson msgpack
                |> Ok

        _ ->
            ("Expected MsgPack's Map or Vector, received - " ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Vector to Elm's List.
-}
toList : MsgPack -> Result Error (List MsgPack)
toList msgpack =
    case msgpack of
        Vector _ value ->
            Ok value

        _ ->
            ("Expected MsgPack's Vector, received -" ++ Basics.toString msgpack)
                |> ConversionFailure
                |> Err


{-| Convert from `MsgPack`'s Text to Elm's String.
-}
toString : MsgPack -> Result Error String
toString msgpack =
    case msgpack of
        Text value ->
            Ok value

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
                        ( Nil
                        , r.bytes
                        )

                    Array_ _ ->
                        parseArray r

                    Bin_ _ ->
                        ( Bin <| List.take r.dataSize r.bytes
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
                        ( Bitwise.and r.dataSize 0xFF
                            |> Bitwise.or 0x000FFFFFFFFFFF00
                            |> Integer
                        , r.bytes
                        )

                    FixPosInt_ _ ->
                        ( Integer r.dataSize
                        , r.bytes
                        )

                    FixStr_ _ ->
                        parseText r

                    Float_ _ ->
                        parseFloat r

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
        Map sz d ->
            Map sz
                (case Dict.get appendKey d of
                    Nothing ->
                        Dict.insert appendKey item d

                    Just kv ->
                        Dict.insert (asKey kv) item d
                            |> Dict.remove appendKey
                )
                |> Ok

        Vector sz l ->
            Vector sz (l ++ [ item ])
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
Convert `MsgPack`'s `value` member to `String`.
-}
asKey : MsgPack -> String
asKey msgpack =
    case msgpack of
        Empty ->
            "__elm-msgpack-empty__"

        Nil ->
            "__elm-msgpack-nil__"

        Bin bytes ->
            Utf8.toString bytes |> Result.withDefault "__elm-msgpack-bin__"

        Boolean value ->
            "__elm-msgpack-bool-" ++ Basics.toString value ++ "__"

        Extension ( _, bytes ) ->
            Utf8.toString bytes |> Result.withDefault "__elm-msgpack-ext__"

        Double value ->
            Basics.toString value

        Integer value ->
            Basics.toString value

        Map _ value ->
            Basics.toString value

        Text value ->
            value

        Vector _ value ->
            Basics.toString value


{-| @private
-}
parseArray : Parsed -> ( MsgPack, List Int )
parseArray r =
    ( Vector r.dataSize []
    , r.bytes
    )


{-| @private
-}
parseBoolean : Parsed -> Bool -> ( MsgPack, List Int )
parseBoolean r flag =
    ( Boolean flag
    , r.bytes
    )


{-| @private
-}
parseExtension : Parsed -> ( MsgPack, List Int )
parseExtension r =
    ( Extension
        ( List.take 1 r.bytes
            |> List.head
            |> Maybe.withDefault 0
        , List.drop 1 r.bytes
            |> List.take (r.dataSize - 1)
        )
    , List.drop r.dataSize r.bytes
    )


{-| @private
-}
parseFloat : Parsed -> ( MsgPack, List Int )
parseFloat r =
    let
        bits =
            r.bytes
                |> List.foldl
                    (\byte str ->
                        str
                            ++ Basics.toString (Bitwise.and byte 0x80 |> Bitwise.shiftRightZfBy 7)
                            ++ Basics.toString (Bitwise.and byte 0x40 |> Bitwise.shiftRightZfBy 6)
                            ++ Basics.toString (Bitwise.and byte 0x20 |> Bitwise.shiftRightZfBy 5)
                            ++ Basics.toString (Bitwise.and byte 0x10 |> Bitwise.shiftRightZfBy 4)
                            ++ Basics.toString (Bitwise.and byte 0x08 |> Bitwise.shiftRightZfBy 3)
                            ++ Basics.toString (Bitwise.and byte 0x04 |> Bitwise.shiftRightZfBy 2)
                            ++ Basics.toString (Bitwise.and byte 0x02 |> Bitwise.shiftRightZfBy 1)
                            ++ Basics.toString (Bitwise.and byte 0x01)
                    )
                    ""

        sign =
            String.slice 0 1 bits
                |> String.toInt
                |> Result.withDefault 0
                |> (\b -> -1 ^ b)
                |> Basics.toFloat

        ( expIdxA, expIdxB, bias ) =
            if r.dataSize == 4 then
                ( 1, 9, -127.0 )
            else
                ( 1, 12, -1023.0 )

        exp =
            String.slice expIdxA expIdxB bits
                |> String.foldr
                    (\b ( a, i ) ->
                        ( if b == '1' then
                            a + (2 ^ i)
                          else
                            0
                        , i + 1
                        )
                    )
                    ( 0, 0 )
                |> (\( num, _ ) -> bias + num)

        fsum =
            String.dropLeft expIdxB bits
                |> String.foldl
                    (\b ( fs, i ) ->
                        if b == '1' then
                            ( fs + (2.0 ^ (-1.0 * i)), i + 1 )
                        else
                            ( fs, i + 1 )
                    )
                    ( 1, 1 )
                |> (\( fs, _ ) -> fs)
    in
    ( Double <| sign * (fsum * (2 ^ exp))
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
    ( Integer value
    , List.drop r.dataSize r.bytes
    )


{-| @private
-}
parseMap : Parsed -> ( MsgPack, List Int )
parseMap r =
    ( Map r.dataSize Dict.empty
    , r.bytes
    )


{-| @private
-}
parseText : Parsed -> ( MsgPack, List Int )
parseText r =
    ( List.take r.dataSize r.bytes
        |> Utf8.toString
        |> Result.withDefault ""
        |> Text
    , List.drop r.dataSize r.bytes
    )


{-| @private
-}
parseUnsigned : Parsed -> ( MsgPack, List Int )
parseUnsigned r =
    ( Integer <| byteValue <| List.take r.dataSize r.bytes
    , List.drop r.dataSize r.bytes
    )



-- ENCODING


{-| Encode to list of bytes.
-}
encode : MsgPack -> List Int
encode msgpack =
    let
        encoder mp accum =
            case mp of
                Empty ->
                    accum

                Nil ->
                    0xC0 :: accum

                Bin bytes ->
                    let
                        binlen =
                            List.length bytes
                    in
                    if binlen <= (2 ^ 8) - 1 then
                        List.reverse bytes ++ intBytes binlen 1 ++ (0xC4 :: accum)
                    else if binlen <= (2 ^ 16) - 1 then
                        List.reverse bytes ++ intBytes binlen 2 ++ (0xC5 :: accum)
                    else if binlen <= (2 ^ 32) - 1 then
                        List.reverse bytes ++ intBytes binlen 4 ++ (0xC6 :: accum)
                    else
                        Debug.log "Too many bytes for Bin, skipping" accum

                Boolean flag ->
                    (if flag == True then
                        0xC3
                     else
                        0xC2
                    )
                        :: accum

                Extension ( t, bytes ) ->
                    let
                        extlen =
                            List.length bytes
                    in
                    if extlen == 1 then
                        bytes ++ (t :: 0xD4 :: accum)
                    else if extlen == 2 then
                        List.reverse bytes ++ (t :: 0xD5 :: accum)
                    else if extlen == 4 then
                        List.reverse bytes ++ (t :: 0xD6 :: accum)
                    else if extlen == 8 then
                        List.reverse bytes ++ (t :: 0xD7 :: accum)
                    else if extlen == 16 then
                        List.reverse bytes ++ (t :: 0xD8 :: accum)
                    else if extlen <= (2 ^ 8) - 1 then
                        List.reverse bytes ++ intBytes extlen 1 ++ (t :: 0xC7 :: accum)
                    else if extlen <= (2 ^ 16) - 1 then
                        List.reverse bytes ++ intBytes extlen 2 ++ (t :: 0xC8 :: accum)
                    else if extlen <= (2 ^ 32) - 1 then
                        List.reverse bytes ++ intBytes extlen 4 ++ (t :: 0xC9 :: accum)
                    else
                        Debug.log "Too many bytes for Ext, skipping" accum

                Double value ->
                    floatBytes value ++ (0xCB :: accum)

                Integer value ->
                    if value >= 0 && value <= 127 then
                        value :: accum
                    else if value >= -31 && value < 0 then
                        Bitwise.and 0xFF value :: accum
                    else if value > 127 && value <= 255 then
                        value :: 0xCC :: accum
                    else if value > 255 && value <= 65535 then
                        Bitwise.and 0xFF value :: (Bitwise.shiftRightZfBy 8 value |> Bitwise.and 0xFF) :: 0xCD :: accum
                    else if value > 65535 && value <= 4294967295 then
                        Bitwise.and 0xFF value
                            :: (Bitwise.shiftRightZfBy 8 value |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 16 value |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 24 value |> Bitwise.and 0xFF)
                            :: 0xCE
                            :: accum
                    else if value >= -127 && value < -31 then
                        Bitwise.and 0xFF value :: 0xD0 :: accum
                    else if value >= -32767 && value < -127 then
                        Bitwise.and 0xFF value :: (Bitwise.shiftRightZfBy 8 value |> Bitwise.and 0xFF) :: 0xD1 :: accum
                    else if value >= -2147483647 && value < -32767 then
                        Bitwise.and 0xFF value
                            :: (Bitwise.shiftRightZfBy 8 value |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 16 value |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 24 value |> Bitwise.and 0xFF)
                            :: 0xD2
                            :: accum
                    else
                        Debug.log "Too big integer values, skipping" accum

                Map sz items ->
                    let
                        itemBytes =
                            encodeItems (Dict.toList items) []

                        encodeItems itemList accum =
                            case itemList of
                                [] ->
                                    accum

                                ( k, i ) :: rest ->
                                    encodeItems rest (encoder i accum ++ encodeText k accum)
                    in
                    if sz > 0 && sz <= 15 then
                        itemBytes ++ (Bitwise.or 0x80 sz :: accum)
                    else if sz <= (2 ^ 16) - 1 then
                        itemBytes ++ (Bitwise.and 0xFF sz :: (Bitwise.shiftRightZfBy 8 sz |> Bitwise.and 0xFF) :: 0xDE :: accum)
                    else if sz <= (2 ^ 32) - 1 then
                        itemBytes
                            ++ Bitwise.and 0xFF sz
                            :: (Bitwise.shiftRightZfBy 8 sz |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 16 sz |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 24 sz |> Bitwise.and 0xFF)
                            :: 0xDF
                            :: accum
                    else
                        accum

                Text value ->
                    encodeText value accum

                Vector sz items ->
                    let
                        itemBytes =
                            encodeItems items []

                        encodeItems itemList accum =
                            case itemList of
                                [] ->
                                    accum

                                i :: rest ->
                                    encodeItems rest (encoder i accum)
                    in
                    if sz > 0 && sz <= 15 then
                        itemBytes ++ (Bitwise.or 0x90 sz :: accum)
                    else if sz <= (2 ^ 16) - 1 then
                        itemBytes ++ (Bitwise.and 0xFF sz :: (Bitwise.shiftRightZfBy 8 sz |> Bitwise.and 0xFF) :: 0xDC :: accum)
                    else if sz <= (2 ^ 32) - 1 then
                        itemBytes
                            ++ Bitwise.and 0xFF sz
                            :: (Bitwise.shiftRightZfBy 8 sz |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 16 sz |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 24 sz |> Bitwise.and 0xFF)
                            :: 0xDD
                            :: accum
                    else
                        accum

        encodeText txt accum =
            let
                bytes =
                    Utf8.toBytes txt

                blen =
                    List.length bytes
            in
            if blen >= 1 && blen <= 31 then
                List.reverse bytes ++ (Bitwise.or 0xA0 blen :: accum)
            else if blen <= (2 ^ 8) - 1 then
                List.reverse bytes ++ (blen :: 0xD9 :: accum)
            else if blen <= (2 ^ 16) - 1 then
                List.reverse bytes ++ (Bitwise.and 0xFF blen :: (Bitwise.shiftRightZfBy 8 blen |> Bitwise.and 0xFF) :: 0xDA :: accum)
            else if blen <= (2 ^ 32) - 1 then
                List.reverse bytes
                    ++ (Bitwise.and 0xFF blen
                            :: (Bitwise.shiftRightZfBy 8 blen |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 16 blen |> Bitwise.and 0xFF)
                            :: (Bitwise.shiftRightZfBy 24 blen |> Bitwise.and 0xFF)
                            :: 0xDB
                            :: accum
                       )
            else
                Debug.log "String too long, skipping" accum
    in
    encoder msgpack []
        |> List.reverse


{-| -}
empty : MsgPack
empty =
    Empty


{-| -}
nil : MsgPack
nil =
    Nil


{-| -}
bin : List Int -> MsgPack
bin list =
    Bin list


{-| -}
bool : Bool -> MsgPack
bool flag =
    Boolean flag


{-| -}
ext : ( Int, List Int ) -> MsgPack
ext t =
    Extension t


{-| -}
float : Float -> MsgPack
float v =
    Double v


{-| -}
int : Int -> MsgPack
int v =
    Integer v


{-| -}
string : String -> MsgPack
string v =
    Text v


{-| -}
list : List MsgPack -> MsgPack
list mpl =
    Vector (List.length mpl) mpl


{-| -}
object : List ( String, MsgPack ) -> MsgPack
object mpl =
    Map (List.length mpl) (Dict.fromList mpl)



-- HTTP


{-| Convert to bytes from a string representation delivered as 'text/plain; charset=x-user-defined'.
-}
asBytes : String -> List Int
asBytes binstr =
    String.toList binstr
        |> List.map (Char.toCode >> Bitwise.and 0xFF)


{-| Convert from bytes to a string representation to be sent with the HTTP
Content-Type header set to 'text/plain; charset=x-user-defined'.
-}
asString : List Int -> String
asString bytes =
    List.map Char.fromCode bytes
        |> String.fromList
