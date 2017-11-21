module MsgPack.Error
    exposing
        ( Error(..)
        )

{-| MsgPack errors.
-}


type Error
    = AppendFailure String
    | EmptyStream
    | ExtTypeMissing
    | IncorrectStart String
    | IncorrectBlockSize String
    | NotImplemented
    | UnknownFormat
