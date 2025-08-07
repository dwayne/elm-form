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
import SignUp.Username as Username exposing (Username)



-- FORM


type alias Form =
    Form.Form Fields Setters Error Output


type alias Fields =
    { username : Field Username.Error Username
    , email : Field Email.Error Email
    }


type alias Setters =
    { setUsername : String -> Fields -> Fields
    , setEmail : String -> Fields -> Fields
    }


type Error
    = UsernameError Username.Error
    | EmailError Email.Error


type alias Output =
    { username : Username
    , email : Email
    }


form : Form
form =
    Form.new
        { setters = setters
        , validate = validate
        }
        { username = Field.empty Username.fieldType
        , email = Field.empty Email.fieldType
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
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    (\username ->
        Output username
    )
        |> Field.succeed
        |> Field.applyValidation (fields.username |> Field.mapError UsernameError)
        |> Field.applyValidation (fields.email |> Field.mapError EmailError)
