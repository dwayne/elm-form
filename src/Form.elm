module Form exposing
    ( Form
    , new
    , Accessor, get, set, modify, update
    , isValid, isInvalid
    , validate, validateAsMaybe, validateAsResult
    , toState
    )

{-| Provides a way to package the fields, accessors, and business logic pertaining to a form behind a consistent interface.


# Form

@docs Form


# Construct

@docs new


# Accessors

@docs Accessor, get, set, modify, update


# Query

@docs isValid, isInvalid


# Validate

@docs validate, validateAsMaybe, validateAsResult


# Convert

@docs toState

-}

import Validation as V exposing (Validation)


{-|

  - `state`: Typically a record containing fields and possibly other forms.
  - `accessors`: Typically a record containing getters/setters and anything else that needs access to the state of the form.
  - `error`: The errors produced by an invalid form.
  - `output`: The data produced by a valid form.

-}
type Form state accessors error output
    = Form
        { state : state
        , accessors : accessors
        , validate : state -> Validation error output
        }



-- CONSTRUCT


{-| -}
new :
    { init : state
    , accessors : accessors
    , validate : state -> Validation error output
    }
    -> Form state accessors error output
new options =
    Form
        { state = options.init
        , accessors = options.accessors
        , validate = options.validate
        }



-- ACCESSORS


{-| Combines a way to `get` a value from a property on `state` and a way to `modify` that property's value.

    type alias State =
        { count : Int
        , name : String
        }

    countAccessor : Accessor State Int
    countAccessor =
        { get = .count
        , modify = \f state -> { state | count = f state.count }
        }

    nameAccessor : Accessor State String
    nameAccessor =
        { get = .name
        , modify = \f state -> { state | name = f state.name }
        }

-}
type alias Accessor state a =
    { get : state -> a
    , modify : (a -> a) -> state -> state
    }


{-| Get the value of a property on the given form.
-}
get : (accessors -> Accessor state a) -> Form state accessors error output -> a
get toAccessor (Form form) =
    (toAccessor form.accessors).get form.state


{-| Set the value of a property on the given form.
-}
set : (accessors -> Accessor state a) -> a -> Form state accessors error output -> Form state accessors error output
set toAccessor x =
    modify toAccessor (always x)


{-| Modify the value of a property on the given form.
-}
modify : (accessors -> Accessor state a) -> (a -> a) -> Form state accessors error output -> Form state accessors error output
modify toAccessor t (Form form) =
    Form { form | state = (toAccessor form.accessors).modify t form.state }


{-| Update the value of a property on the given form.
-}
update : (accessors -> state -> state) -> Form state accessors error output -> Form state accessors error output
update f (Form form) =
    Form { form | state = f form.accessors form.state }



-- QUERY


{-| `True` if the form does not have any errors.
-}
isValid : Form state accessors error output -> Bool
isValid =
    validate >> V.isValid


{-| `True` if the form has errors. `isInvalid form` is equivalent to `not (isValid form)`.
-}
isInvalid : Form state accessors error output -> Bool
isInvalid =
    not << isValid



-- VALIDATE


{-| -}
validate : Form state accessors error output -> Validation error output
validate (Form form) =
    form.validate form.state


{-| -}
validateAsResult : Form state accessors error output -> Result (List error) output
validateAsResult =
    validate >> V.toResult


{-| -}
validateAsMaybe : Form state accessors error output -> Maybe output
validateAsMaybe =
    validate >> V.toMaybe



-- CONVERT


{-| -}
toState : Form state accessors error output -> state
toState (Form { state }) =
    state
