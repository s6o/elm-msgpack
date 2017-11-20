module MsgPack
    exposing
        ( Error(..)
        , MsgPack
        , asBytes
        , toMsgPack
        , toString
        )

{-| MsgPack byte stream handling.


# HTTP


## Response

@docs asBytes


## Request

@docs toString


# Parsing

@docs toMsgPack, MsgPack, Error

-}

import Array
import Bitwise
import Char
import Result exposing (Result)


{-| Convert to bytes from an iso-8859-1 string representation.
-}
asBytes : String -> List Int
asBytes bstr =
    String.toList bstr
        |> List.map (Char.toCode >> Bitwise.and 0xFF)


{-| Convert from bytes to string representation to be sent with the HTTP
Content-Type header as 'text/plain; charset=iso-8859-1'.
-}
toString : List Int -> String
toString bstr =
    "TODO"


{-| Elm's MessagePack byte-stream wrapper.
-}
type MsgPack
    = Nil Int
    | Array_ Int ArrayRecord
    | Bin Int BinRecord
    | Bool_ Int Bool
    | Ext Int ExtRecord
    | Float_ Int (ValueRecord Float)
    | Integer Int (ValueRecord Int)
    | Map Int MapRecord
    | Str Int (ValueRecord String)
    | UInt Int (ValueRecord Int)


{-| MessagePack parsing error.
-}
type Error
    = AppendFailure String
    | EmptyStream
    | IncorrectStart String
    | IncorrectBlockSize String
    | NotImplemented


{-| `MsgPack` type size indicator for parse helpers
-}
type BlockBytes
    = B0
    | B1
    | B2
    | B4
    | B8
    | B16


type alias ArrayRecord =
    { block : BlockBytes
    , total : Int
    , count : Int
    , objects : List MsgPack
    }


type alias BinRecord =
    { block : BlockBytes
    , bsize : Int
    , bytes : List Int
    }


type alias ExtRecord =
    { block : BlockBytes
    , fixed : Bool
    , type_ : Int
    , data : List Int
    , dsize : Int
    }


type alias MapRecord =
    { block : BlockBytes
    , total : Int
    , count : Int
    , objects : List ( MsgPack, MsgPack )
    }


type alias ValueRecord x =
    { block : BlockBytes
    , signed : Bool
    , value : x
    }


type FlagBits
    = None
    | Single
    | Triple
    | Nibble


flagBits : FlagBits -> Int
flagBits fb =
    case fb of
        None ->
            0

        Single ->
            1

        Triple ->
            3

        Nibble ->
            4


{-| Parse a list of bytes into `MsgPack`.
-}
toMsgPack : List Int -> Result Error MsgPack
toMsgPack bytes =
    let
        parse blist accum =
            case blist of
                [] ->
                    Result.fromMaybe EmptyStream accum

                b :: list ->
                    case parseByte b list accum of
                        ( Err error, _ ) ->
                            Err error

                        ( Ok mp, stream ) ->
                            parse stream <| Just mp
    in
    parse bytes Nothing


{-| @private
-}
parseByte : Int -> List Int -> Maybe MsgPack -> ( Result Error MsgPack, List Int )
parseByte b bytes accum =
    case b of
        0xC0 ->
            -- nil
            ( parseNil b
                |> append accum
            , bytes
            )

        0xC2 ->
            -- false
            ( parseBool b False
                |> append accum
            , bytes
            )

        0xC3 ->
            -- true
            ( parseBool b True
                |> append accum
            , bytes
            )

        0xC4 ->
            -- bin8
            parseBin b B1 bytes

        0xC5 ->
            -- bin16
            parseBin b B2 bytes

        0xC6 ->
            -- bin32
            parseBin b B4 bytes

        0xC7 ->
            -- ext8
            ( Err NotImplemented, bytes )

        0xC8 ->
            -- ext16
            ( Err NotImplemented, bytes )

        0xC9 ->
            -- ext32
            ( Err NotImplemented, bytes )

        0xCA ->
            -- float32
            let
                ( msgpack, nextBytes ) =
                    parseFloat b B4 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xCB ->
            -- float64
            let
                ( msgpack, nextBytes ) =
                    parseFloat b B8 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xCC ->
            -- uint8
            let
                ( msgpack, nextBytes ) =
                    parseIntUnsigned b B1 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xCD ->
            -- uint16
            let
                ( msgpack, nextBytes ) =
                    parseIntUnsigned b B2 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xCE ->
            -- uint32
            let
                ( msgpack, nextBytes ) =
                    parseIntUnsigned b B4 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xCF ->
            -- uint64
            let
                ( msgpack, nextBytes ) =
                    parseIntUnsigned b B8 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xD0 ->
            -- int8
            let
                ( msgpack, nextBytes ) =
                    parseInt b B1 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xD1 ->
            -- int16
            let
                ( msgpack, nextBytes ) =
                    parseInt b B2 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xD2 ->
            -- int32
            let
                ( msgpack, nextBytes ) =
                    parseInt b B4 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xD3 ->
            -- int64
            let
                ( msgpack, nextBytes ) =
                    parseInt b B8 bytes
            in
            ( append accum msgpack
            , nextBytes
            )

        0xD4 ->
            -- fixext1
            ( Err NotImplemented, bytes )

        0xD5 ->
            -- fixext2
            ( Err NotImplemented, bytes )

        0xD6 ->
            -- fixext4
            ( Err NotImplemented, bytes )

        0xD7 ->
            -- fixext8
            ( Err NotImplemented, bytes )

        0xD8 ->
            -- fixext16
            ( Err NotImplemented, bytes )

        0xD9 ->
            -- str8
            ( Err NotImplemented, bytes )

        0xDA ->
            -- str16
            ( Err NotImplemented, bytes )

        0xDB ->
            -- str32
            ( Err NotImplemented, bytes )

        0xDC ->
            -- array16
            ( parseArray b B2 bytes
            , bytes
            )

        0xDD ->
            -- array32
            ( parseArray b B4 bytes
            , bytes
            )

        0xDE ->
            -- map16
            ( parseMap b B2 bytes
            , bytes
            )

        0xDF ->
            -- map32
            ( parseMap b B4 bytes
            , bytes
            )

        _ ->
            if b >= 0x00 && b <= 0x7F then
                -- pos fixint
                let
                    ( msgpack, nextBytes ) =
                        parseIntUnsigned b B0 bytes
                in
                ( append accum msgpack
                , nextBytes
                )
            else if b >= 0x80 && b <= 0x8F then
                -- fixmap
                ( parseMap b B0 bytes
                , bytes
                )
            else if b >= 0x90 && b <= 0x9F then
                -- fixarray
                ( parseArray b B0 bytes
                , bytes
                )
            else if b >= 0xA0 && b <= 0xBF then
                -- fixstr
                ( Err NotImplemented, bytes )
            else if b >= 0xE0 && b <= 0xFF then
                -- neg fixint
                let
                    ( msgpack, nextBytes ) =
                        parseInt b B0 bytes
                in
                ( append accum msgpack
                , nextBytes
                )
            else
                ( Err NotImplemented, bytes )



-- PARSING HELPERS


{-| @private
-}
blockBytesCount : BlockBytes -> Int
blockBytesCount block =
    case block of
        B0 ->
            0

        B1 ->
            1

        B2 ->
            2

        B4 ->
            4

        B8 ->
            8

        B16 ->
            16


{-| @private
-}
dataCount : Int -> FlagBits -> BlockBytes -> List Int -> Int
dataCount byte fb block stream =
    if block == B0 then
        let
            leftBits =
                Bitwise.shiftRightBy (8 - flagBits fb) byte
                    |> Bitwise.shiftLeftBy (8 - flagBits fb)
        in
        Bitwise.xor byte leftBits
    else if block == B1 then
        byte
    else
        stream
            |> List.take (blockBytesCount block)
            |> List.foldr
                (\b ( a, bi ) -> ( a + Bitwise.shiftLeftBy (bi * 8) b, bi + 1 ))
                ( 0, 0 )
            |> (\( a, _ ) -> a)


{-| Parse a MessagePack nil from byte stream.
-}
parseNil : Int -> Result Error MsgPack
parseNil byte =
    Nil byte |> Ok


{-| Parse a MessagePack bool from byte stream.
-}
parseBool : Int -> Bool -> Result Error MsgPack
parseBool byte flag =
    Bool_ byte flag |> Ok


{-| Parse a MessagePack bin from byte stream.
-}
parseBin : Int -> BlockBytes -> List Int -> ( Result Error MsgPack, List Int )
parseBin byte block stream =
    let
        validBlocks =
            [ B1, B2, B4 ]

        byteCount =
            dataCount byte Nibble block stream
    in
    if List.member block validBlocks then
        ( Bin
            byte
            { block = block
            , bsize = byteCount
            , bytes = List.take byteCount stream
            }
            |> Ok
        , List.drop byteCount stream
        )
    else
        ( ("Unsuppored bin type (size): " ++ Basics.toString block)
            |> IncorrectBlockSize
            |> Err
        , stream
        )


{-| Parse a MessagePack float from byte stream.
-}
parseFloat : Int -> BlockBytes -> List Int -> ( Result Error MsgPack, List Int )
parseFloat byte block stream =
    let
        validBlocks =
            [ B4, B8 ]
    in
    if List.member block validBlocks then
        let
            rawBits =
                dataCount byte None block stream
        in
        case block of
            B4 ->
                let
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
                ( Float_
                    byte
                    { block = block
                    , signed = False
                    , value = sign * (fsum * (2 ^ exp))
                    }
                    |> Ok
                , List.drop (blockBytesCount block) stream
                )

            B8 ->
                let
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
                ( Float_
                    byte
                    { block = block
                    , signed = False
                    , value = sign * (fsum * (2 ^ exp))
                    }
                    |> Ok
                , List.drop (blockBytesCount block) stream
                )

            _ ->
                ( "Incorrect internal validBlocks configuration."
                    |> IncorrectBlockSize
                    |> Err
                , stream
                )
    else
        ( ("Unsupported float type (size): " ++ Basics.toString block)
            |> IncorrectBlockSize
            |> Err
        , stream
        )


{-| Parse a MessagePack int from byte stream.
-}
parseInt : Int -> BlockBytes -> List Int -> ( Result Error MsgPack, List Int )
parseInt byte block stream =
    let
        validBlocks =
            [ B0, B1, B2, B4, B8 ]
    in
    if List.member block validBlocks then
        ( Integer
            byte
            { block = block
            , signed = True
            , value = dataCount byte Triple block stream
            }
            |> Ok
        , List.drop (blockBytesCount block) stream
        )
    else
        ( ("Unsupported unsigned int type (size): " ++ Basics.toString block)
            |> IncorrectBlockSize
            |> Err
        , stream
        )


{-| Parse a MessagePack uint from byte stream.
-}
parseIntUnsigned : Int -> BlockBytes -> List Int -> ( Result Error MsgPack, List Int )
parseIntUnsigned byte block stream =
    let
        validBlocks =
            [ B0, B1, B2, B4, B8 ]
    in
    if List.member block validBlocks then
        ( UInt
            byte
            { block = block
            , signed = False
            , value = dataCount byte Single block stream
            }
            |> Ok
        , List.drop (blockBytesCount block) stream
        )
    else
        ( ("Unsupported unsigned int type (size): " ++ Basics.toString block)
            |> IncorrectBlockSize
            |> Err
        , stream
        )


{-| Parse a MessagePack array from byte stream.

Supported MessagePack types: fixarray, array 16, array 32.

-}
parseArray : Int -> BlockBytes -> List Int -> Result Error MsgPack
parseArray byte block stream =
    let
        validBlocks =
            [ B0, B2, B4 ]
    in
    if List.member block validBlocks then
        Array_
            byte
            { block = block
            , total = dataCount byte Nibble block stream
            , count = 0
            , objects = []
            }
            |> Ok
    else
        ("Unsupported array type (size): " ++ Basics.toString block)
            |> IncorrectBlockSize
            |> Err


{-| Parse a MessagePack map from byte stream.

Supported MessagePack types: fixmap, map 16, map 32.

-}
parseMap : Int -> BlockBytes -> List Int -> Result Error MsgPack
parseMap byte block stream =
    let
        validBlocks =
            [ B0, B2, B4 ]
    in
    if List.member block validBlocks then
        Map
            byte
            { block = block
            , total = 2 * dataCount byte Nibble block stream
            , count = 0
            , objects = []
            }
            |> Ok
    else
        ("Unsupported map type (size): " ++ Basics.toString block)
            |> IncorrectBlockSize
            |> Err


{-| Append a `MsgPack` item to `MsgPack` collection (array or map).

All collection items must have the same type. First item added to the collection
sets the type of the collection.

-}
append : Maybe MsgPack -> Result Error MsgPack -> Result Error MsgPack
append collection item =
    let
        updateArray b r i =
            Array_
                b
                { r
                    | count = r.count + 1
                    , objects =
                        if r.total > r.count + 1 then
                            i :: r.objects
                        else
                            i :: r.objects |> List.reverse
                }

        updateMapKey b r k =
            Map b { r | count = r.count + 1, objects = ( k, k ) :: r.objects }

        updateMapValue b r v =
            Map
                b
                { r
                    | count = r.count + 1
                    , objects =
                        if r.total > r.count + 1 then
                            case r.objects of
                                [] ->
                                    r.objects

                                ( k, _ ) :: rest ->
                                    ( k, v ) :: rest
                        else
                            case r.objects of
                                [] ->
                                    r.objects

                                ( k, _ ) :: rest ->
                                    ( k, v ) :: rest |> List.reverse
                }
    in
    case collection of
        Just (Array_ b r) ->
            if r.count < r.total then
                case
                    ( List.head r.objects
                    , item
                    )
                of
                    ( Nothing, Ok i ) ->
                        updateArray b r i |> Ok

                    ( Just headItem, Ok i ) ->
                        if sameType headItem i then
                            updateArray b r i |> Ok
                        else
                            "Array item type mismatch."
                                |> AppendFailure
                                |> Err

                    ( Nothing, Err error ) ->
                        Err error

                    ( Just _, Err error ) ->
                        Err error
            else
                "Array full, expected size of items exceeded."
                    |> AppendFailure
                    |> Err

        Just (Map b r) ->
            if r.count < r.total then
                case
                    ( List.head r.objects
                    , item
                    )
                of
                    ( Nothing, Ok i ) ->
                        updateMapKey b r i |> Ok

                    ( Just ( k, v ), Ok i ) ->
                        if (r.count + 1) % 2 == 1 then
                            -- odd, map key
                            if sameType k i then
                                updateMapKey b r i |> Ok
                            else
                                "Map key type mismatch."
                                    |> AppendFailure
                                    |> Err
                        else if sameType v i then
                            updateMapValue b r i |> Ok
                        else
                            "Map value type mismatch."
                                |> AppendFailure
                                |> Err

                    ( Nothing, Err error ) ->
                        Err error

                    ( Just _, Err error ) ->
                        Err error
            else
                "Map full, expected size of items exceeded."
                    |> AppendFailure
                    |> Err

        _ ->
            "First argument must be a `MsgPack` collection: array or map."
                |> AppendFailure
                |> Err


{-| @private
-}
sameType : MsgPack -> MsgPack -> Bool
sameType a b =
    case ( a, b ) of
        ( Nil _, Nil _ ) ->
            True

        ( Array_ _ _, Array_ _ _ ) ->
            True

        ( Bin _ _, Bin _ _ ) ->
            True

        ( Bool_ _ _, Bool_ _ _ ) ->
            True

        ( Ext _ _, Ext _ _ ) ->
            True

        ( Float_ _ _, Float_ _ _ ) ->
            True

        ( Integer _ _, Integer _ _ ) ->
            True

        ( Map _ _, Map _ _ ) ->
            True

        ( Str _ _, Str _ _ ) ->
            True

        ( UInt _ _, UInt _ _ ) ->
            True

        _ ->
            False
