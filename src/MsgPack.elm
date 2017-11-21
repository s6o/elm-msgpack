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
                            parse next <| Just mp
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


{-| Append a `MsgPack` item to `MsgPack` collection (array or map).

All collection items must have the same type. First item added to the collection
sets the type of the collection.

-}



{- }
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
-}


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


{-| @private
-}
sameType : MsgPack -> MsgPack -> Bool
sameType a b =
    case ( a, b ) of
        ( Nil _, Nil _ ) ->
            True

        ( Blob _, Blob _ ) ->
            True

        ( Boolean _, Boolean _ ) ->
            True

        ( Extension _, Extension _ ) ->
            True

        ( Double _, Double _ ) ->
            True

        ( Integer _, Integer _ ) ->
            True

        ( Map _, Map _ ) ->
            True

        ( Text _, Text _ ) ->
            True

        ( Vector _, Vector _ ) ->
            True

        _ ->
            False
