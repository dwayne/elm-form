module FormList.Error exposing (textErrorToString)

import Data.Text as Text
import Field.Advanced as F


textErrorToString : Text.Error -> String
textErrorToString =
    F.errorToString
        { onBlank = "It is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \error ->
                case error of
                    Text.TooShort min ->
                        "It must be at least " ++ String.fromInt min ++ " characters in length."
        }
