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
import MsgPack exposing (BlockBytes(..), Error(..), MsgPack)
import Result


{-| Convert to bytes from ISO-8859-1 string representation.
-}
toBytes : String -> List Int
toBytes bstr =
    String.toList bstr
        |> List.map (Char.toCode >> Bitwise.and 0xFF)


{-| Parse a list of bytes into `MsgPack`.
-}
toMsgPack : List Int -> Result Error MsgPack
toMsgPack bytes =
    let
        parse blist accum =
            case blist of
                [] ->
                    Result.fromMaybe MsgPack.EmptyStream accum

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
            ( MsgPack.parseNil b
                |> MsgPack.append accum
            , bytes
            )

        0xC2 ->
            -- false
            ( MsgPack.parseBool b False
                |> MsgPack.append accum
            , bytes
            )

        0xC3 ->
            -- true
            ( MsgPack.parseBool b True
                |> MsgPack.append accum
            , bytes
            )

        0xC4 ->
            -- bin8
            MsgPack.parseBin b B1 bytes

        0xC5 ->
            -- bin16
            MsgPack.parseBin b B2 bytes

        0xC6 ->
            -- bin32
            MsgPack.parseBin b B4 bytes

        0xC7 ->
            -- ext8
            ( Err MsgPack.NotImplemented, bytes )

        0xC8 ->
            -- ext16
            ( Err MsgPack.NotImplemented, bytes )

        0xC9 ->
            -- ext32
            ( Err MsgPack.NotImplemented, bytes )

        0xCA ->
            -- float32
            ( Err MsgPack.NotImplemented, bytes )

        0xCB ->
            -- float64
            ( Err MsgPack.NotImplemented, bytes )

        0xCC ->
            -- uint8
            ( Err MsgPack.NotImplemented, bytes )

        0xCD ->
            -- uint16
            ( Err MsgPack.NotImplemented, bytes )

        0xCE ->
            -- uint32
            ( Err MsgPack.NotImplemented, bytes )

        0xCF ->
            -- uint64
            ( Err MsgPack.NotImplemented, bytes )

        0xD0 ->
            -- int8
            ( Err MsgPack.NotImplemented, bytes )

        0xD1 ->
            -- int16
            ( Err MsgPack.NotImplemented, bytes )

        0xD2 ->
            -- int32
            ( Err MsgPack.NotImplemented, bytes )

        0xD3 ->
            -- int64
            ( Err MsgPack.NotImplemented, bytes )

        0xD4 ->
            -- fixext1
            ( Err MsgPack.NotImplemented, bytes )

        0xD5 ->
            -- fixext2
            ( Err MsgPack.NotImplemented, bytes )

        0xD6 ->
            -- fixext4
            ( Err MsgPack.NotImplemented, bytes )

        0xD7 ->
            -- fixext8
            ( Err MsgPack.NotImplemented, bytes )

        0xD8 ->
            -- fixext16
            ( Err MsgPack.NotImplemented, bytes )

        0xD9 ->
            -- str8
            ( Err MsgPack.NotImplemented, bytes )

        0xDA ->
            -- str16
            ( Err MsgPack.NotImplemented, bytes )

        0xDB ->
            -- str32
            ( Err MsgPack.NotImplemented, bytes )

        0xDC ->
            -- array16
            ( MsgPack.parseArray b B2 bytes
            , bytes
            )

        0xDD ->
            -- array32
            ( MsgPack.parseArray b B4 bytes
            , bytes
            )

        0xDE ->
            -- map16
            ( MsgPack.parseMap b B2 bytes
            , bytes
            )

        0xDF ->
            -- map32
            ( MsgPack.parseMap b B4 bytes
            , bytes
            )

        _ ->
            if b >= 0x00 && b <= 0x7F then
                -- pos fixint
                ( Err MsgPack.NotImplemented, bytes )
            else if b >= 0x80 && b <= 0x8F then
                -- fixmap
                ( MsgPack.parseMap b B0 bytes
                , bytes
                )
            else if b >= 0x90 && b <= 0x9F then
                -- fixarray
                ( MsgPack.parseArray b B0 bytes
                , bytes
                )
            else if b >= 0xA0 && b <= 0xBF then
                -- fixstr
                ( Err MsgPack.NotImplemented, bytes )
            else if b >= 0xE0 && b <= 0xFF then
                -- neg fixint
                ( Err MsgPack.NotImplemented, bytes )
            else
                ( Err MsgPack.NotImplemented, bytes )
