module SignUp.Error exposing
    ( emailErrorToString
    , passwordConfirmationErrorToString
    , passwordErrorToString
    , signUpErrorToString
    , usernameErrorToString
    )

import Field.Advanced as F
import SignUp.Email as Email
import SignUp.Form as SignUp
import SignUp.Password as Password
import SignUp.PasswordConfirmation as PasswordConfirmation
import SignUp.Username as Username


usernameErrorToString : Username.Error -> String
usernameErrorToString =
    F.errorToString
        { onBlank = "It is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \error ->
                case error of
                    Username.TooShort { actual, min } ->
                        "It must be at least " ++ String.fromInt min ++ " characters in length: " ++ String.fromInt actual ++ "."

                    Username.TooLong { actual, max } ->
                        "It must be at most " ++ String.fromInt max ++ " characters in length: " ++ String.fromInt actual ++ "."
        }


emailErrorToString : Email.Error -> String
emailErrorToString =
    F.errorToString
        { onBlank = "It is required."
        , onSyntaxError = always "It is not an email address."
        , onValidationError = always ""
        , onCustomError = always ""
        }


passwordErrorToString : Password.Error -> String
passwordErrorToString =
    F.errorToString
        { onBlank = "It is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \error ->
                case error of
                    Password.TooShort { actual, min } ->
                        "It must be at least " ++ String.fromInt min ++ " characters in length: " ++ String.fromInt actual ++ "."

                    Password.MissingRequiredChars requiredChars ->
                        "It must contain at least one of each of the following: a lowercase character, an uppercase character, a number, and a special character in the set \"(" ++ requiredChars ++ ")\"."
        }


passwordConfirmationErrorToString : PasswordConfirmation.Error -> String
passwordConfirmationErrorToString =
    F.errorToString
        { onBlank = "It is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \PasswordConfirmation.Mismatch ->
                "It does not match the password."
        }


signUpErrorToString : SignUp.Error -> String
signUpErrorToString error =
    case error of
        SignUp.UsernameError e ->
            usernameErrorToString e

        SignUp.EmailError e ->
            emailErrorToString e

        SignUp.PasswordError e ->
            passwordErrorToString e

        SignUp.PasswordConfirmationError e ->
            passwordConfirmationErrorToString e
