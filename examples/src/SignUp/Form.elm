module SignUp.Form exposing
    ( Error(..)
    , Form
    , Modifiers
    , Output
    , State
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
    Form.Form State Modifiers Error Output


type alias State =
    { username : Field Username.Error Username
    , email : Field Email.Error Email
    , password : Field Password.Error Password
    , passwordConfirmation : Field PasswordConfirmation.Error PasswordConfirmation
    }


type alias Modifiers =
    { setUsername : String -> State -> State
    , setEmail : String -> State -> State
    , setPassword : String -> State -> State
    , setPasswordConfirmation : String -> State -> State
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
        { init = init
        , modifiers = modifiers
        , validate = validate
        }



-- INIT


init : State
init =
    { username = Field.empty Username.fieldType
    , email = Field.empty Email.fieldType
    , password = Field.empty Password.fieldType
    , passwordConfirmation = Field.empty PasswordConfirmation.fieldType
    }



-- MODIFIERS


modifiers : Modifiers
modifiers =
    { setUsername =
        \s state ->
            { state | username = Field.setFromString s state.username }
    , setEmail =
        \s state ->
            { state | email = Field.setFromString s state.email }
    , setPassword =
        \s state ->
            let
                password =
                    Field.setFromString s state.password
            in
            { state | password = password, passwordConfirmation = updatePasswordConfirmation password state.passwordConfirmation }
    , setPasswordConfirmation =
        \s state ->
            let
                passwordConfirmation =
                    Field.setFromString s state.passwordConfirmation
            in
            { state | passwordConfirmation = updatePasswordConfirmation state.password passwordConfirmation }
    }


updatePasswordConfirmation :
    Field Password.Error Password
    -> Field PasswordConfirmation.Error PasswordConfirmation
    -> Field PasswordConfirmation.Error PasswordConfirmation
updatePasswordConfirmation password passwordConfirmation =
    (\p pc ->
        if Password.toString p == PasswordConfirmation.toString pc then
            passwordConfirmation

        else
            Field.setCustomError PasswordConfirmation.Mismatch passwordConfirmation
    )
        |> Just
        |> Field.applyMaybe password
        |> Field.applyMaybe passwordConfirmation
        |> Maybe.withDefault passwordConfirmation



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    (\username email password _ ->
        Output username email password
    )
        |> Field.succeed
        |> Field.applyValidation (state.username |> Field.mapError UsernameError)
        |> Field.applyValidation (state.email |> Field.mapError EmailError)
        |> Field.applyValidation (state.password |> Field.mapError PasswordError)
        |> Field.applyValidation (state.passwordConfirmation |> Field.mapError PasswordConfirmationError)
