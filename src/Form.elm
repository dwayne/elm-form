module Form exposing
    ( Form
    , Options
    , isInvalid
    , isValid
    , new
    , toState
    , update
    , validate
    , validateAsMaybe
    , validateAsResult
    )

import Validation as V exposing (Validation)


type Form state modifiers error output
    = Form
        { config : Config state modifiers error output
        , state : state
        }


type alias Config state modifiers error output =
    { modifiers : modifiers
    , validate : state -> Validation error output
    }



-- CONSTRUCT


type alias Options state modifiers error output =
    { init : state
    , modifiers : modifiers
    , validate : state -> Validation error output
    }


new : Options state modifiers error output -> Form state modifiers error output
new options =
    Form
        { config =
            { modifiers = options.modifiers
            , validate = options.validate
            }
        , state = options.init
        }



-- MODIFY


update : (modifiers -> a -> state -> state) -> a -> Form state modifiers error output -> Form state modifiers error output
update f x (Form form) =
    Form { form | state = f form.config.modifiers x form.state }



-- QUERY


isValid : Form state modifiers error output -> Bool
isValid =
    validate >> V.isValid


isInvalid : Form state modifiers error output -> Bool
isInvalid =
    not << isValid



-- VALIDATE


validate : Form state modifiers error output -> Validation error output
validate (Form { config, state }) =
    config.validate state


validateAsResult : Form state modifiers error output -> Result (List error) output
validateAsResult =
    validate >> V.toResult


validateAsMaybe : Form state modifiers error output -> Maybe output
validateAsMaybe =
    validate >> V.toMaybe



-- CONVERT


toState : Form state modifiers error output -> state
toState (Form { state }) =
    state
