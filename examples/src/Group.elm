module Group exposing
    ( Accessors
    , Error
    , Form
    , Output
    , State
    , form
    )

import Field exposing (Field)
import Form exposing (Accessor)
import Form.List exposing (Forms, Id)
import Person
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { name : Field String
    , description : Field String
    , people : Forms Person.Form
    }


type alias Accessors =
    { name : Accessor State (Field String)
    , description : Accessor State (Field String)
    , people : Accessor State (Forms Person.Form)
    , addPerson : State -> State
    , removePerson : Id -> State -> State
    }


type Error
    = NameError Field.Error
    | DescriptionError Field.Error
    | PersonError Id Person.Error


type alias Output =
    { name : String
    , description : String
    , people : List Person.Output
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
    { name = Field.empty Field.nonBlankString
    , description = Field.empty Field.nonBlankString
    , people = Form.List.empty
    }



-- ACCESSORS


accessors : Accessors
accessors =
    { name =
        { get = .name
        , modify = \f state -> { state | name = f state.name }
        }
    , description =
        { get = .description
        , modify = \f state -> { state | description = f state.description }
        }
    , people =
        { get = .people
        , modify = \f state -> { state | people = f state.people }
        }
    , addPerson = \state -> { state | people = Form.List.append Person.form state.people }
    , removePerson = \id state -> { state | people = Form.List.remove id state.people }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Output
        |> Field.succeed
        |> Field.applyValidation (state.name |> Field.mapError NameError)
        |> Field.applyValidation (state.description |> Field.mapError DescriptionError)
        |> V.apply (Form.List.validate PersonError state.people)
