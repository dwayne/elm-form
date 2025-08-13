module DynamicForm.Form.Post exposing
    ( Error
    , Fields
    , Form
    , Output
    , Setters
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field, Validation)
import Form



-- FORM


type alias Form =
    Form.Form Fields Setters Error Output


type alias Fields =
    { body : Field Text.Error Text
    }


type alias Setters =
    { setBody : String -> Fields -> Fields
    }


type Error
    = BodyError Text.Error


type alias Output =
    { body : Text
    }


form : Form
form =
    Form.new
        { setters = setters
        , validate = validate
        }
        { body = Field.empty (Text.fieldType 10)
        }



-- SETTERS


setters : Setters
setters =
    { setBody =
        \s fields ->
            { fields | body = Field.setFromString s fields.body }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    Field.validate Output (fields.body |> Field.mapError BodyError)
