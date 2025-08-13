module DynamicForm.Error exposing
    ( publicationErrorToString
    , textErrorToString
    )

import Data.Text as Text
import DynamicForm.Publication as Publication
import Field.Advanced as F


publicationErrorToString : Publication.Error -> String
publicationErrorToString =
    F.errorToString
        { onBlank = "It is required."
        , onSyntaxError = always ""
        , onValidationError = \s -> "It is not a type of publication: " ++ s ++ "."
        , onCustomError = always ""
        }


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
