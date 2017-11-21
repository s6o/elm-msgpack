module MsgPack.Format
    exposing
        ( Data
        , Format(..)
        , Parsed
        , byteValue
        , format
        , parse
        )

{-| MessagePack specification.

@docs Data, Format, Parsed, byteValue, format, parse

-}

import Bitwise
import MsgPack.Error as Error exposing (Error)
import Result exposing (Result)


{-| How is data length to be determined for a given `Format`.
-}
type Data
    = Blocks Int Flag
    | BlocksType Int Int Flag
    | Bytes Int Flag
    | Empty Flag
    | Fixed FlagMasked
    | TypeBytes Int Int Flag


{-| MessagePack format marker byte.
-}
type Flag
    = Flag Int


{-| MessagePack format marker byte with a bit mask.
-}
type FlagMasked
    = FlagMasked Int Int


{-| MessagePack formats.

MessagePack's positive fixnum and uint types are captured by `Unsigned` and
the negative fixnum and int types are captured by `Integer`.

-}
type Format
    = Nil Data
    | Array_ Data
    | Bin Data
    | Ext Data
    | False_ Data
    | FixArray Data
    | FixExt Data
    | FixMap Data
    | FixStr Data
    | Float_ Data
    | Map Data
    | Integer Data
    | Str Data
    | True_ Data
    | Unsigned Data


{-| Parsed bytes, header removed for given `Format`.
-}
type alias Parsed =
    { format : Format
    , bytes : List Int
    , dataSize : Int
    }


{-| @private
Number of bytes used for block specifing data length.
-}
blockCount : Data -> Int
blockCount data =
    case data of
        Blocks c _ ->
            c

        BlocksType c _ _ ->
            c

        _ ->
            0


{-| Given a number of bytes in big-endian, construct an integer value.
-}
byteValue : List Int -> Int
byteValue bytes =
    bytes
        |> List.foldr
            (\b ( a, bi ) -> ( a + Bitwise.shiftLeftBy (bi * 8) b, bi + 1 ))
            ( 0, 0 )
        |> (\( a, _ ) -> a)


{-| @private
Get data layout from `Format`.
-}
data : Format -> Data
data fmt =
    case fmt of
        Nil d ->
            d

        Array_ d ->
            d

        Bin d ->
            d

        Ext d ->
            d

        False_ d ->
            d

        FixArray d ->
            d

        FixExt d ->
            d

        FixMap d ->
            d

        FixStr d ->
            d

        Float_ d ->
            d

        Map d ->
            d

        Integer d ->
            d

        Str d ->
            d

        True_ d ->
            d

        Unsigned d ->
            d


{-| @private
Number of data bytes accordingly `Format`'s data layout.
-}
dataLength : Data -> List Int -> Int
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

        Empty _ ->
            0

        Fixed (FlagMasked byte mask) ->
            Bitwise.and byte mask

        TypeBytes t c _ ->
            t + c


{-| Detect MessagePack `Format` from specified byte value.
-}
format : Int -> Maybe Format
format byte =
    case byte of
        0xC0 ->
            -- nil
            Just <| Nil <| Empty <| Flag byte

        0xC2 ->
            -- false
            Just <| False_ <| Empty <| Flag byte

        0xC3 ->
            -- true
            Just <| True_ <| Empty <| Flag byte

        0xC4 ->
            -- bin8
            Just <| Bin <| Blocks 1 <| Flag byte

        0xC5 ->
            -- bin16
            Just <| Bin <| Blocks 1 <| Flag byte

        0xC6 ->
            -- bin32
            Just <| Bin <| Blocks 1 <| Flag byte

        0xC7 ->
            -- ext8
            Just <| Ext <| BlocksType 1 1 <| Flag byte

        0xC8 ->
            -- ext16
            Just <| Ext <| BlocksType 2 1 <| Flag byte

        0xC9 ->
            -- ext32
            Just <| Ext <| BlocksType 4 1 <| Flag byte

        0xCA ->
            -- float32
            Just <| Float_ <| Bytes 4 <| Flag byte

        0xCB ->
            -- float64
            Just <| Float_ <| Bytes 8 <| Flag byte

        0xCC ->
            -- uint8
            Just <| Unsigned <| Bytes 1 <| Flag byte

        0xCD ->
            -- uint16
            Just <| Unsigned <| Bytes 2 <| Flag byte

        0xCE ->
            -- uint32
            Just <| Unsigned <| Bytes 4 <| Flag byte

        0xCF ->
            -- uint64
            Just <| Unsigned <| Bytes 8 <| Flag byte

        0xD0 ->
            -- int8
            Just <| Integer <| Bytes 1 <| Flag byte

        0xD1 ->
            -- int16
            Just <| Integer <| Bytes 2 <| Flag byte

        0xD2 ->
            -- int32
            Just <| Integer <| Bytes 4 <| Flag byte

        0xD3 ->
            Just <| Integer <| Bytes 8 <| Flag byte

        0xD4 ->
            -- fixext1
            Just <| FixExt <| TypeBytes 1 1 <| Flag byte

        0xD5 ->
            -- fixext2
            Just <| FixExt <| TypeBytes 1 2 <| Flag byte

        0xD6 ->
            -- fixext4
            Just <| FixExt <| TypeBytes 1 4 <| Flag byte

        0xD7 ->
            -- fixext8
            Just <| FixExt <| TypeBytes 1 8 <| Flag byte

        0xD8 ->
            -- fixext16
            Just <| FixExt <| TypeBytes 1 16 <| Flag byte

        0xD9 ->
            -- str8
            Just <| Str <| Blocks 1 <| Flag byte

        0xDA ->
            -- str16
            Just <| Str <| Blocks 2 <| Flag byte

        0xDB ->
            -- str32
            Just <| Str <| Blocks 4 <| Flag byte

        0xDC ->
            -- array16
            Just <| Array_ <| Blocks 2 <| Flag byte

        0xDD ->
            -- array32
            Just <| Array_ <| Blocks 4 <| Flag byte

        0xDE ->
            -- map16
            Just <| Map <| Blocks 2 <| Flag byte

        0xDF ->
            -- map32
            Just <| Map <| Blocks 4 <| Flag byte

        _ ->
            if byte >= 0x00 && byte <= 0x7F then
                -- pos fixint
                Just <| Unsigned <| Fixed <| FlagMasked byte 0x7F
            else if byte >= 0x80 && byte <= 0x8F then
                -- fixmap
                Just <| Map <| Fixed <| FlagMasked byte 0x0F
            else if byte >= 0x90 && byte <= 0x9F then
                -- fixarray
                Just <| Array_ <| Fixed <| FlagMasked byte 0x0F
            else if byte >= 0xA0 && byte <= 0xBF then
                -- fixstr
                Just <| Str <| Fixed <| FlagMasked byte 0x1F
            else if byte >= 0xE0 && byte <= 0xFF then
                -- neg fixint
                Just <| Integer <| Fixed <| FlagMasked byte 0x1F
            else
                Nothing


{-| Parse chunk of data bytes accordingly to `Format`.
-}
parse : Maybe Format -> List Int -> Result Error Parsed
parse fmt bytes =
    fmt
        |> Maybe.map
            (\f ->
                let
                    d =
                        data f
                in
                { format = f
                , bytes = List.drop (1 + blockCount d) bytes
                , dataSize = dataLength d bytes
                }
            )
        |> Result.fromMaybe Error.UnknownFormat
