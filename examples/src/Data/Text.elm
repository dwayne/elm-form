module Data.Text exposing
    ( CustomError(..)
    , Error
    , Text
    , fieldType
    , fromString
    , toString
    )

import Field.Advanced as F


type Text
    = Text String


type alias Error =
    F.Error CustomError


type CustomError
    = TooShort Int


fromString : Int -> String -> Result Error Text
fromString rawMinChars =
    let
        minChars =
            --
            -- Ensure minChars >= 1
            --
            max 1 rawMinChars
    in
    F.trim
        (\s ->
            if String.length s < minChars then
                Err (F.customError <| TooShort minChars)

            else
                Ok (Text s)
        )


toString : Text -> String
toString (Text s) =
    s


fieldType : Int -> F.Type Error Text
fieldType minChars =
    F.customType
        { fromString = fromString minChars
        , toString = toString
        }
