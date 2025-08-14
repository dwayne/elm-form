module DynamicForm.Form.Question exposing
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
    { title : Field Text.Error Text
    , body : Field Text.Error (Maybe Text)
    }


type alias Modifiers =
    { setTitle : String -> State -> State
    , setBody : String -> State -> State
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
        , modifiers = modifiers
        , validate = validate
        }



-- INIT


init : State
init =
    { title = Field.empty (Text.fieldType 10)
    , body = Field.empty (Field.optional <| Text.fieldType 100)
    }



-- MODIFIERS


modifiers : Modifiers
modifiers =
    { setTitle =
        \s state ->
            { state | title = Field.setFromString s state.title }
    , setBody =
        \s state ->
            { state | body = Field.setFromString s state.body }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Output
        |> Field.succeed
        |> Field.applyValidation (state.title |> Field.mapError TitleError)
        |> Field.applyValidation (state.body |> Field.mapError BodyError)
