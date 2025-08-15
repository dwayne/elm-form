module DynamicForm.Form.Post exposing
    ( Accessors
    , Error
    , Form
    , Output
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field, Validation)
import Form3 as Form exposing (Accessor)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { body : Field Text.Error Text
    }


type alias Accessors =
    { body : Accessor State (Field Text.Error Text)
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
        , accessors = accessors
        , validate = validate
        }



-- INIT


init : State
init =
    { body = Field.empty (Text.fieldType 10)
    }



-- ACCESSORS


accessors : Accessors
accessors =
    { body =
        { get = .body
        , modify = \f state -> { state | body = f state.body }
        }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Field.validate Output (state.body |> Field.mapError BodyError)
