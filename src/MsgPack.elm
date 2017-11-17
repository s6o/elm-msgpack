module MsgPack
    exposing
        ( BlockBytes(..)
        , Error(..)
        , MsgPack
        , append
        , parseArray
        , parseBin
        , parseBool
        , parseMap
        , parseNil
        )

{-| Types for MessagePack specification.


# Specification type

@docs MsgPack, Error


# Decoding helpers

@docs BlockBytes


## Collections

@docs parseArray, parseMap, append


## Types

@docs parseNil, parseBool, parseBin

-}

import Bitwise
import Result exposing (Result)


{-| Elm's MessagePack byte-stream wrapper.
-}
type MsgPack
    = Nil Int
    | Array_ Int ArrayRecord
    | Bin Int BinRecord
    | Bool_ Int Bool
    | Ext Int ExtRecord
    | Float_ Int ValueRecord
    | Integer Int ValueRecord
    | Map Int MapRecord
    | Str Int ValueRecord
    | UInt Int ValueRecord


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


type alias ValueRecord =
    { block : BlockBytes
    , signed : Bool
    , bytes : List Int
    }



-- Parsing Helpers


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
dataCount : Int -> BlockBytes -> List Int -> Int
dataCount byte block stream =
    if block == B0 then
        let
            leftNibble =
                Bitwise.shiftRightBy 4 byte
        in
        Bitwise.xor byte leftNibble
    else if block == B1 then
        byte
    else
        stream
            |> List.take (blockBytesCount block)
            |> List.foldl (\b a -> Bitwise.shiftLeftBy 8 b) 0


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
            dataCount byte block stream
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
        ( ("Unsuppored bin type (size): " ++ toString block)
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
            , total = dataCount byte block stream
            , count = 0
            , objects = []
            }
            |> Ok
    else
        ("Unsupported array type (size): " ++ toString block)
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
            , total = 2 * dataCount byte block stream
            , count = 0
            , objects = []
            }
            |> Ok
    else
        ("Unsupported map type (size): " ++ toString block)
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
