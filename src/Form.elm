module Form exposing
    ( Form
    , new
    , Accessor
    , get
    , set, modify, update
    , isValid, isInvalid
    , validate, validateAsMaybe, validateAsResult
    , toState
    )

{-|


# Form

@docs Form


# Construct

@docs new


# Accessors

@docs Accessor
@docs get
@docs set, modify, update


# Query

@docs isValid, isInvalid


# Validate

@docs validate, validateAsMaybe, validateAsResult


# Convert

@docs toState

-}

import Validation as V exposing (Validation)


{-| -}
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


{-| -}
type alias Accessor state a =
    { get : state -> a
    , modify : (a -> a) -> state -> state
    }


{-| -}
get : (accessors -> Accessor state a) -> Form state accessors error output -> a
get toAccessor (Form form) =
    (toAccessor form.accessors).get form.state


{-| -}
set : (accessors -> Accessor state a) -> a -> Form state accessors error output -> Form state accessors error output
set toAccessor x =
    modify toAccessor (always x)


{-| -}
modify : (accessors -> Accessor state a) -> (a -> a) -> Form state accessors error output -> Form state accessors error output
modify toAccessor t (Form form) =
    Form { form | state = (toAccessor form.accessors).modify t form.state }


{-| -}
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
