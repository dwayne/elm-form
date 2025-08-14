module AddressForm exposing
    ( Form
    , Lenses
    , State
    , form
    )

import Form2 as Form
import Lens exposing (Lens)


type alias Form =
    Form.Form State Lenses


type alias State =
    { country : String
    , population : Int
    }


type alias Lenses =
    { country : Lens State String
    , population : Lens State Int
    }


form : Form
form =
    Form.new
        { country = "Trinidad"
        , population = 100
        }
        { country =
            { get = .country
            , modify = \f state -> { state | country = f state.country }
            }
        , population =
            { get = .population
            , modify =
                \f state ->
                    let
                        newPopulation =
                            f state.population
                    in
                    if newPopulation > 1000 then
                        { state | country = state.country ++ " & Tobago", population = newPopulation }

                    else
                        { state | population = newPopulation }
            }
        }
