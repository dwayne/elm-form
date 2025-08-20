# elm-form

A lightweight form abstraction that helps you model the non-UI related aspects of a form via a consistent interface.

## Examples

### A person form

Collect a person's first name and optional last name.

```elm
module Person exposing
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
```

Usage in the repl:

```elm
import Field
import Form
import Person

Person.form
    |> Form.modify .firstName (Field.setFromString "Dave")
    |> Form.validateAsMaybe
-- Just "Dave"

Person.form
    |> Form.modify .firstName (Field.setFromString "Dave")
    |> Form.modify .lastName (Field.setFromString "MacQueen")
    |> Form.validateAsMaybe
-- Just "Dave MacQueen"

Person.form
    |> Form.modify .firstName (Field.setFromString "\t Dave  \t ")
    |> Form.get .firstName
    |> Field.toRawString
-- "\t Dave  \t "

Person.form
    |> Form.modify .firstName (Field.setFromString "\t Dave  \t ")
    |> Form.get .firstName
    |> Field.toString
-- "Dave"

Person.form
    |> Form.validateAsResult
-- Err [FirstNameError Blank]
```

### A group form

Collect the name, description, and people of a group. It reuses the person form.

```elm
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
```

Usage in the repl:

```elm
import Field
import Form
import Form.List
import Group

--
-- Based on the UChicago Programming Languages Group:
--
-- https://cs.uchicago.edu/research/programming-languages/
--

name =
    "Programming Languages Group"

description =
    "Interested in all aspects of programming language design and implementation, ranging from theoretical foundations to practical applications."

group =
    Group.form
        |> Form.modify .name (Field.setFromString name)
        |> Form.modify .description (Field.setFromString description)

groupWithPeople =
    group
        |> Form.update .addPerson
        |> Form.update .addPerson
        |> Form.update .addPerson
        |> Form.modify .people
            (Form.List.modify 0 .firstName (Field.setFromString "Ravi")
                >> Form.List.modify 0 .lastName (Field.setFromString "Chugh")
            )
        |> Form.modify .people
            (Form.List.modify 1 .firstName (Field.setFromString "Robert")
                >> Form.List.modify 1 .lastName (Field.setFromString "Rand")
            )
        |> Form.modify .people
            (Form.List.modify 2 .firstName (Field.setFromString "John")
                >> Form.List.modify 2 .lastName (Field.setFromString "Reppy")
            )

groupWithPeople
    |> Form.validateAsMaybe
-- Just
--     { name = name
--     , description = description
--     , people =
--         [ "Ravi Chugh"
--         , "Robert Rand"
--         , "John Reppy"
--         ]
--     }

groupWithPeople
    |> Form.update (\r -> r.removePerson 1)
    |> Form.validateAsMaybe
-- Just
--     { name = name
--     , description = description
--     , people =
--         [ "Ravi Chugh"
--         , "John Reppy"
--         ]
--     }
```

For many more examples you can look at the [unit tests for the package](/tests/Test), check out the [`examples/`](/examples) directory, play with the [live demo](https://dwayne.github.io/elm-form/), and/or explore the [unit tests for the examples](/examples/tests/Test).
