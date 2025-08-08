module SignUp.Form exposing
    ( Error
    , Fields
    , Form
    , Output
    , Setters
    , form
    )

import Field.Advanced as Field exposing (Field, Validation)
import Form
import SignUp.Email as Email exposing (Email)
import SignUp.Password as Password exposing (Password)
import SignUp.PasswordConfirmation as PasswordConfirmation exposing (PasswordConfirmation)
import SignUp.Username as Username exposing (Username)



-- FORM


type alias Form =
    Form.Form Fields Setters Error Output


type alias Fields =
    { username : Field Username.Error Username
    , email : Field Email.Error Email
    , password : Field Password.Error Password
    , passwordConfirmation : Field PasswordConfirmation.Error PasswordConfirmation
    }


type alias Setters =
    { setUsername : String -> Fields -> Fields
    , setEmail : String -> Fields -> Fields
    , setPassword : String -> Fields -> Fields
    , setPasswordConfirmation : String -> Fields -> Fields
    }


type Error
    = UsernameError Username.Error
    | EmailError Email.Error
    | PasswordError Password.Error
    | PasswordConfirmationError PasswordConfirmation.Error


type alias Output =
    { username : Username
    , email : Email
    , password : Password
    }


form : Form
form =
    Form.new
        { setters = setters
        , validate = validate
        }
        { username = Field.empty Username.fieldType
        , email = Field.empty Email.fieldType
        , password = Field.empty Password.fieldType
        , passwordConfirmation = Field.empty PasswordConfirmation.fieldType
        }



-- SETTERS


setters : Setters
setters =
    { setUsername =
        \s fields ->
            { fields | username = Field.setFromString s fields.username }
    , setEmail =
        \s fields ->
            { fields | email = Field.setFromString s fields.email }
    , setPassword =
        \s fields ->
            let
                password =
                    Field.setFromString s fields.password

                maybeIsMatching =
                    (\p pc ->
                        Password.toString p == PasswordConfirmation.toString pc
                    )
                        |> Just
                        |> Field.applyMaybe password
                        |> Field.applyMaybe fields.passwordConfirmation
            in
            if maybeIsMatching == Just False then
                { fields | password = password, passwordConfirmation = Field.setCustomError PasswordConfirmation.Mismatch fields.passwordConfirmation }

            else
                { fields | password = password }
    , setPasswordConfirmation =
        \s fields ->
            let
                passwordConfirmation =
                    Field.setFromString s fields.passwordConfirmation

                maybeIsMatching =
                    (\p pc ->
                        Password.toString p == PasswordConfirmation.toString pc
                    )
                        |> Just
                        |> Field.applyMaybe fields.password
                        |> Field.applyMaybe passwordConfirmation
            in
            if maybeIsMatching == Just False then
                { fields | passwordConfirmation = Field.setCustomError PasswordConfirmation.Mismatch passwordConfirmation }

            else
                { fields | passwordConfirmation = passwordConfirmation }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    (\username email password _ ->
        Output username email password
    )
        |> Field.succeed
        |> Field.applyValidation (fields.username |> Field.mapError UsernameError)
        |> Field.applyValidation (fields.email |> Field.mapError EmailError)
        |> Field.applyValidation (fields.password |> Field.mapError PasswordError)
        |> Field.applyValidation (fields.passwordConfirmation |> Field.mapError PasswordConfirmationError)
