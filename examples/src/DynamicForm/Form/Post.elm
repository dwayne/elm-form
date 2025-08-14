module DynamicForm.Form.Post exposing
    ( Error
    , Form
    , Modifiers
    , Output
    , State
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field, Validation)
import Form



-- FORM


type alias Form =
    Form.Form State Modifiers Error Output


type alias State =
    { body : Field Text.Error Text
    }


type alias Modifiers =
    { setBody : String -> State -> State
    }


type Error
    = BodyError Text.Error


type alias Output =
    { body : Text
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
    { body = Field.empty (Text.fieldType 10)
    }



-- MODIFIERS


modifiers : Modifiers
modifiers =
    { setBody =
        \s state ->
            { state | body = Field.setFromString s state.body }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Field.validate Output (state.body |> Field.mapError BodyError)
