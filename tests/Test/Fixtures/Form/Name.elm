module Test.Fixtures.Form.Name exposing
    ( Error(..)
    , Form
    , Modifiers
    , Output
    , State
    , form
    )

import Field exposing (Field, Validation)
import Form



-- FORM


type alias Form =
    Form.Form State Modifiers Error Output


type alias State =
    { firstName : Field String
    , lastName : Field (Maybe String)
    }


type alias Modifiers =
    { setFirstName : String -> State -> State
    , setLastName : String -> State -> State
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
        , modifiers = modifiers
        , validate = validate
        }



-- INIT


init : State
init =
    { firstName = Field.empty Field.nonBlankString
    , lastName = Field.empty (Field.optional Field.nonBlankString)
    }



-- MODIFIERS


modifiers : Modifiers
modifiers =
    { setFirstName =
        \s state ->
            { state | firstName = Field.setFromString s state.firstName }
    , setLastName =
        \s state ->
            { state | lastName = Field.setFromString s state.lastName }
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
