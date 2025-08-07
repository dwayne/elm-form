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
import SignUp.Username as Username exposing (Username)



-- FORM


type alias Form =
    Form.Form Fields Setters Error Output


type alias Fields =
    { username : Field Username.Error Username
    }


type alias Setters =
    { setUsername : String -> Fields -> Fields
    }


type Error
    = UsernameError Username.Error


type alias Output =
    { username : Username
    }


form : Form
form =
    Form.new
        { setters = setters
        , validate = validate
        }
        { username = Field.empty Username.fieldType
        }



-- SETTERS


setters : Setters
setters =
    { setUsername =
        \s fields ->
            { fields | username = Field.setFromString s fields.username }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    (\username ->
        Output username
    )
        |> Field.succeed
        |> Field.applyValidation (fields.username |> Field.mapError UsernameError)
