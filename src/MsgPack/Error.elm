module MsgPack.Error
    exposing
        ( Error(..)
        )

{-| MsgPack errors.
-}


type Error
    = AppendFailure String
    | EmptyStream
    | NotImplemented
    | UnknownFormat
