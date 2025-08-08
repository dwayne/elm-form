module SignUp.Username exposing
    ( CustomError(..)
    , Error
    , Username
    , fieldType
    , fromString
    , toString
    )

import Field.Advanced as F


type Username
    = Username String


type alias Error =
    F.Error CustomError


type CustomError
    = TooShort { actual : Int, min : Int }
    | TooLong { actual : Int, max : Int }


fromString : String -> Result Error Username
fromString =
    F.trim
        (\s ->
            let
                n =
                    String.length s
            in
            if n < 3 then
                Err (F.customError <| TooShort { actual = n, min = 3 })

            else if n > 25 then
                Err (F.customError <| TooLong { actual = n, max = 25 })

            else
                Ok (Username s)
        )


toString : Username -> String
toString (Username s) =
    s


fieldType : F.Type Error Username
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
