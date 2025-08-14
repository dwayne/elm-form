module Test.Fixtures.Form.Name exposing
    ( Accessors
    , Error(..)
    , Form
    , Output
    , State
    , form
    )

import Field exposing (Field, Validation)
import Form exposing (Accessor)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { firstName : Field String
    , lastName : Field (Maybe String)
    }


type alias Accessors =
    { firstName : Accessor State (Field String)
    , lastName : Accessor State (Field (Maybe String))
    }


type Error
    = FirstNameError Field.Error
    | LastNameError Field.Error


type alias Output =
    String


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
    { firstName = Field.empty Field.nonBlankString
    , lastName = Field.empty (Field.optional Field.nonBlankString)
    }



-- ACCESSSORS


accessors : Accessors
accessors =
    { firstName =
        { get = .firstName
        , modify = \f state -> { state | firstName = f state.firstName }
        }
    , lastName =
        { get = .lastName
        , modify = \f state -> { state | lastName = f state.lastName }
        }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    (\firstName maybeLastName ->
        case maybeLastName of
            Just lastName ->
                firstName ++ " " ++ lastName

            Nothing ->
                firstName
    )
        |> Field.succeed
        |> Field.applyValidation (state.firstName |> Field.mapError FirstNameError)
        |> Field.applyValidation (state.lastName |> Field.mapError LastNameError)
