module Lenses exposing (..)

--
-- Based on https://academy.fpblock.com/haskell/tutorial/lens/
--


type alias Person =
    { name : String
    , address : Address
    }


type alias Address =
    { city : String
    , street : String
    }


alice : Person
alice =
    { name = "Alice"
    , address =
        { city = "A city"
        , street = "A street"
        }
    }


getCity : Person -> String
getCity =
    .address >> .city



--
-- 1. The straightforward approach
--


setCity1 : String -> Person -> Person
setCity1 newCity person =
    let
        address =
            person.address

        newAddress =
            { address | city = newCity }
    in
    { person | address = newAddress }



--
-- 2. Modifier functions
--


modifyAddressCity : (String -> String) -> Address -> Address
modifyAddressCity f address =
    { address | city = f address.city }


modifyPersonAddress : (Address -> Address) -> Person -> Person
modifyPersonAddress f person =
    { person | address = f person.address }


modifyPersonCity : (String -> String) -> Person -> Person
modifyPersonCity =
    modifyPersonAddress << modifyAddressCity


setCity2 : String -> Person -> Person
setCity2 newCity =
    modifyPersonCity (always newCity)



--
-- 3. Old style lenses
--


type Lens s a
    = Lens
        { getter : s -> a
        , modify : (a -> a) -> s -> s
        }
