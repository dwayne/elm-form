module DynamicForm.Form.Question exposing
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
    { title : Field Text.Error Text
    , body : Field Text.Error (Maybe Text)
    }


type alias Setters =
    { setTitle : String -> Fields -> Fields
    , setBody : String -> Fields -> Fields
    }


type Error
    = TitleError Text.Error
    | BodyError Text.Error


type alias Output =
    { title : Text
    , body : Maybe Text
    }


form : Form
form =
    Form.new
        { setters = setters
        , validate = validate
        }
        { title = Field.empty (Text.fieldType 10)
        , body = Field.empty (Field.optional <| Text.fieldType 100)
        }



-- SETTERS


setters : Setters
setters =
    { setTitle =
        \s fields ->
            { fields | title = Field.setFromString s fields.title }
    , setBody =
        \s fields ->
            { fields | body = Field.setFromString s fields.body }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    Output
        |> Field.succeed
        |> Field.applyValidation (fields.title |> Field.mapError TitleError)
        |> Field.applyValidation (fields.body |> Field.mapError BodyError)
