module PersonForm exposing
    ( Form
    , Lenses
    , State
    , form
    )

import AddressForm
import Form2 as Form
import Lens exposing (Lens)


type alias Form =
    Form.Form State Lenses


type alias State =
    { name : String
    , age : Int
    , addressForm : AddressForm.Form
    }


type alias Lenses =
    { name : Lens State String
    , age : Lens State Int
    , country : Lens State String
    , population : Lens State Int
    }


form : Form
form =
    Form.new
        { name = "Dwayne"
        , age = 25
        , addressForm = AddressForm.form
        }
        { name =
            { get = .name
            , modify =
                \f state ->
                    let
                        newName =
                            f state.name
                    in
                    if newName == "Denzil" then
                        { state | name = newName, addressForm = Form.set .country "Washington" state.addressForm }

                    else
                        { state | name = newName }
            }
        , age =
            { get = .age
            , modify = \f state -> { state | age = f state.age }
            }
        , country =
            { get = .addressForm >> Form.get .country
            , modify = \f state -> { state | addressForm = Form.modify .country f state.addressForm }
            }
        , population =
            { get = .addressForm >> Form.get .population
            , modify = \f state -> { state | addressForm = Form.modify .population f state.addressForm }
            }
        }
