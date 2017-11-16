module MsgPack
    exposing
        ( MsgPack
        )

{-| Types for MessagePack specification.

@docs MsgPack

-}

import Result exposing (Result)


type MsgPack
    = Nil Int
    | False_ Int
    | True_ Int
    | Array_ Int Container
    | Bin Int BinRecord
    | Ext Int ExtRecord
    | Float_ Int ValueRecord
    | Integer Int ValueRecord
    | Map Int Container
    | Str Int ValueRecord
    | UInt Int ValueRecord


type BlockBytes
    = B0
    | B1
    | B2
    | B4
    | B8
    | B16
    | B32
    | B64


type alias BinRecord =
    { block : BlockBytes
    , bytes : List Int
    , bsize : Int
    }


type alias Container =
    { block : BlockBytes
    , objects : List MsgPack
    }


type alias ExtRecord =
    { block : BlockBytes
    , fixed : Bool
    , type_ : Int
    , data : List Int
    , dsize : Int
    }


type alias ValueRecord =
    { block : BlockBytes
    , signed : Bool
    , bytes : List Int
    }


toByte : MsgPack -> Result String Int
toByte msgpack =
    case msgpack of
        Nil _ ->
            Ok 0xC0

        False_ _ ->
            Ok 0xC2

        True_ _ ->
            Ok 0xC3

        Array_ _ { block } ->
            case block of
                B0 ->
                    -- fix: 1001xxxx 0x90 - 0x9f
                    Ok 0x90

                B16 ->
                    Ok 0xDC

                B32 ->
                    Ok 0xDD

                _ ->
                    Err "Invalid Array_ BlockBytes value. Allowed values: B0, B16, B32."

        Bin _ { block } ->
            case block of
                B8 ->
                    Ok 0xC4

                B16 ->
                    Ok 0xC5

                B32 ->
                    Ok 0xC6

                _ ->
                    Err "Invalid Bin BlockBytes value. Allowed values: B8, B16 or B32."

        Ext _ { block, fixed } ->
            if fixed then
                case block of
                    B1 ->
                        Ok 0xD4

                    B2 ->
                        Ok 0xD5

                    B4 ->
                        Ok 0xD6

                    B8 ->
                        Ok 0xD7

                    B16 ->
                        Ok 0xD8

                    _ ->
                        Err "Invalid fixed Ext BlockBytes values. Allowed values: B1, B2, B4, B8, B16."
            else
                case block of
                    B8 ->
                        Ok 0xC7

                    B16 ->
                        Ok 0xC8

                    B32 ->
                        Ok 0xC9

                    _ ->
                        Err "Invalid variable Ext BlockBytes value. Allowed values: B8, B16, B16."

        Float_ _ { block } ->
            case block of
                B32 ->
                    Ok 0xCA

                B64 ->
                    Ok 0xCB

                _ ->
                    Err "Invalid Float BlockBytes value. Allowed values: B32, B64."

        Integer _ { block, signed } ->
            case block of
                B0 ->
                    if signed then
                        -- fix: positive fixint 0xxxxxxx 0x00 - 0x7f
                        Ok 0x00
                    else
                        -- negative fixint 111xxxxx 0xe0 - 0xff
                        Ok 0xE0

                B8 ->
                    Ok 0xD0

                B16 ->
                    Ok 0xD1

                B32 ->
                    Ok 0xD2

                B64 ->
                    Ok 0xD3

                _ ->
                    Err "Invalid Integer BlockBytes value. Allowed values: B0, B8, B16, B32, B64"

        Map _ { block } ->
            case block of
                B0 ->
                    -- fix: 1000xxxx 0x80 - 0x8f
                    Ok 0x80

                B16 ->
                    Ok 0xDE

                B32 ->
                    Ok 0xDF

                _ ->
                    Err "Invalid Map BlockBytes value. Allowed values: B0, B16, B32."

        Str _ { block, bytes } ->
            case block of
                B0 ->
                    -- fix this, bit mask with size: 0xA0 - 0xBF
                    Ok 0xA0

                B8 ->
                    Ok 0xD9

                B16 ->
                    Ok 0xDA

                B32 ->
                    Ok 0xDB

                _ ->
                    Err "Invalid Str BlockBytes value. Allowed values: B0, B8, B16, B32"

        UInt _ { block } ->
            case block of
                B8 ->
                    Ok 0xCC

                B16 ->
                    Ok 0xCD

                B32 ->
                    Ok 0xCE

                B64 ->
                    Ok 0xCF

                _ ->
                    Err "Invalid Integer BlockBytes value. Allowed values: B8, B16, B32, B64"
