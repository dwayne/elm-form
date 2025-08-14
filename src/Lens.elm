module Lens exposing (Lens)


type alias Lens s a =
    { get : s -> a
    , modify : (a -> a) -> s -> s
    }


compose : Lens a b -> Lens b c -> Lens a c
compose lens1 lens2 =
    { get = lens1.get >> lens2.get
    , modify = lens1.modify << lens2.modify
    }
