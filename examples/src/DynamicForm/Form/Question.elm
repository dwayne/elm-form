module DynamicForm.Form.Question exposing
    ( Accessors
    , Error
    , Form
    , Output
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field, Validation)
import Form exposing (Accessor)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { title : Field Text.Error Text
    , body : Field Text.Error (Maybe Text)
    }


type alias Accessors =
    { title : Accessor State (Field Text.Error Text)
    , body : Accessor State (Field Text.Error (Maybe Text))
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
        { init = init
        , accessors = accessors
        , validate = validate
        }



-- INIT


init : State
init =
    { title = Field.empty (Text.fieldType 10)
    , body = Field.empty (Field.optional <| Text.fieldType 100)
    }



-- ACCESSORS


accessors : Accessors
accessors =
    { title =
        { get = .title
        , modify = \f state -> { state | title = f state.title }
        }
    , body =
        { get = .body
        , modify = \f state -> { state | body = f state.body }
        }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Output
        |> Field.succeed
        |> Field.applyValidation (state.title |> Field.mapError TitleError)
        |> Field.applyValidation (state.body |> Field.mapError BodyError)
