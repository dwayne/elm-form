module Form3 exposing
    ( Accessor
    , Form
    , Options
    , compose
    , get
    , isInvalid
    , isValid
    , modify
    , new
    , set
    , validate
    , validateAsMaybe
    , validateAsResult
    )

import Validation as V exposing (Validation)


type Form state accessors error output
    = Form
        { state : state
        , accessors : accessors
        , validate : state -> Validation error output
        }



-- CONSTRUCT


type alias Options state accessors error output =
    { init : state
    , accessors : accessors
    , validate : state -> Validation error output
    }


new : Options state accessors error output -> Form state accessors error output
new options =
    Form
        { state = options.init
        , accessors = options.accessors
        , validate = options.validate
        }



-- ACCESSOR


type alias Accessor s a =
    { get : s -> a
    , modify : (a -> a) -> s -> s
    }


compose : Accessor a b -> Accessor b c -> Accessor a c
compose x y =
    { get = x.get >> y.get
    , modify = x.modify << y.modify
    }



-- GET


get : (accessors -> Accessor state a) -> Form state accessors error output -> a
get toAccessor (Form form) =
    (toAccessor form.accessors).get form.state



-- MODIFY


modify : (accessors -> Accessor state a) -> (a -> a) -> Form state accessors error output -> Form state accessors error output
modify toAccessor t (Form form) =
    Form { form | state = (toAccessor form.accessors).modify t form.state }


set : (accessors -> Accessor state a) -> a -> Form state accessors error output -> Form state accessors error output
set toAccessor x =
    modify toAccessor (always x)



-- QUERY


isValid : Form state accessors error output -> Bool
isValid =
    validate >> V.isValid


isInvalid : Form state accessors error output -> Bool
isInvalid =
    not << isValid



-- VALIDATE


validate : Form state accessors error output -> Validation error output
validate (Form form) =
    form.validate form.state


validateAsResult : Form state accessors error output -> Result (List error) output
validateAsResult =
    validate >> V.toResult


validateAsMaybe : Form state accessors error output -> Maybe output
validateAsMaybe =
    validate >> V.toMaybe
