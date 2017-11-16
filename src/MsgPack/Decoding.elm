module MsgPack.Decoding
    exposing
        ( toBytes
        )

{-| MsgPack decoder.


# HTTP response

@docs toBytes

-}

import Bitwise
import Char
import MsgPack exposing (MsgPack)


{-| Convert to bytes from ISO-8859-1 string representation.
-}
toBytes : String -> List Int
toBytes bstr =
    String.toList bstr
        |> List.map (Char.toCode >> Bitwise.and 0xFF)


{-| Parse a list of bytes into `MsgPack`.
-}
toMsgPack : List Int -> Maybe MsgPack
toMsgPack bytes =
    let
        parse blist accum =
            case blist of
                [] ->
                    accum

                b :: list ->
                    parseByte b list accum
                        |> parse list
    in
    parse bytes Nothing


{-| @private
-}
parseByte : Int -> List Int -> Maybe MsgPack -> Maybe MsgPack
parseByte b bytes msgpack =
    case b of
        0xC0 ->
            -- nil
            Nothing

        0xC2 ->
            -- false
            Nothing

        0xC3 ->
            -- true
            Nothing

        0xC4 ->
            -- bin8
            Nothing

        0xC5 ->
            -- bin16
            Nothing

        0xC6 ->
            -- bin32
            Nothing

        0xC7 ->
            -- ext8
            Nothing

        0xC8 ->
            -- ext16
            Nothing

        0xC9 ->
            -- ext32
            Nothing

        0xCA ->
            -- float32
            Nothing

        0xCB ->
            -- float64
            Nothing

        0xCC ->
            -- uint8
            Nothing

        0xCD ->
            -- uint16
            Nothing

        0xCE ->
            -- uint32
            Nothing

        0xCF ->
            -- uint64
            Nothing

        0xD0 ->
            -- int8
            Nothing

        0xD1 ->
            -- int16
            Nothing

        0xD2 ->
            -- int32
            Nothing

        0xD3 ->
            -- int64
            Nothing

        0xD4 ->
            -- fixext1
            Nothing

        0xD5 ->
            -- fixext2
            Nothing

        0xD6 ->
            -- fixext4
            Nothing

        0xD7 ->
            -- fixext8
            Nothing

        0xD8 ->
            -- fixext16
            Nothing

        0xD9 ->
            -- str8
            Nothing

        0xDA ->
            -- str16
            Nothing

        0xDB ->
            -- str32
            Nothing

        0xDC ->
            -- array16
            Nothing

        0xDD ->
            -- array32
            Nothing

        0xDE ->
            -- map16
            Nothing

        0xDF ->
            -- map32
            Nothing

        _ ->
            if b >= 0x00 && b <= 0x7F then
                -- pos fixint
                Nothing
            else if b >= 0x80 && b <= 0x8F then
                -- fixmap
                Nothing
            else if b >= 0x90 && b <= 0x9F then
                -- fixarray
                Nothing
            else if b >= 0xA0 && b <= 0xBF then
                -- fixstr
                Nothing
            else if b >= 0xE0 && b <= 0xFF then
                -- neg fixint
                Nothing
            else
                Nothing
