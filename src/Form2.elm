module Form2 exposing
    ( Form
    , get
    , modify
    , new
    , set
    )

import Lens exposing (Lens)


type Form state lenses
    = Form
        { state : state
        , lenses : lenses
        }


new : state -> lenses -> Form state lenses
new state lenses =
    Form { state = state, lenses = lenses }


get : (lenses -> Lens state a) -> Form state lenses -> a
get toLens (Form form) =
    (toLens form.lenses).get form.state


modify : (lenses -> Lens state a) -> (a -> a) -> Form state lenses -> Form state lenses
modify toLens t (Form form) =
    Form { form | state = (toLens form.lenses).modify t form.state }


set : (lenses -> Lens state a) -> a -> Form state lenses -> Form state lenses
set toLens x =
    modify toLens (always x)
