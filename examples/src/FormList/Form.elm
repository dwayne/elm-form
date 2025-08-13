module FormList.Form exposing
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
    { name : Field Text.Error Text
    }


type alias Setters =
    { setName : String -> Fields -> Fields
    }


type Error
    = NameError Text.Error


type alias Output =
    { name : Text
    }


form : Form
form =
    Form.new
        { setters = setters
        , validate = validate
        }
        { name = Field.empty (Text.fieldType 2)
        }



-- SETTERS


setters : Setters
setters =
    { setName =
        \s fields ->
            { fields | name = Field.setFromString s fields.name }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    Field.validate Output (fields.name |> Field.mapError NameError)
