module FormList.Website exposing
    ( Accessors
    , Error
    , Form
    , Output
    , State
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field, Validation)
import Form exposing (Accessor)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { name : Field Text.Error Text
    , address : Field Text.Error Text
    }


type alias Accessors =
    { name : Accessor State (Field Text.Error Text)
    , address : Accessor State (Field Text.Error Text)
    }


type Error
    = NameError Text.Error
    | AddressError Text.Error


type alias Output =
    { name : Text
    , address : Text
    }


form : String -> String -> Form
form name address =
    Form.new
        { init = init name address
        , accessors = accessors
        , validate = validate
        }



-- INIT


init : String -> String -> State
init name address =
    { name = Field.fromString (Text.fieldType 1) name
    , address = Field.fromString (Text.fieldType 1) address
    }



-- ACCESSORS


accessors : Accessors
accessors =
    { name =
        { get = .name
        , modify = \f state -> { state | name = f state.name }
        }
    , address =
        { get = .address
        , modify = \f state -> { state | address = f state.address }
        }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Field.validate2
        Output
        (state.name |> Field.mapError NameError)
        (state.address |> Field.mapError AddressError)
