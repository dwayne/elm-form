module FormList.Form exposing
    ( Error
    , Form
    , Modifiers
    , Output
    , State
    , Website
    , WebsiteState
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field)
import Form
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form State Modifiers Error Output


type alias State =
    { name : Field Text.Error Text
    , websites : List WebsiteState
    }


type alias WebsiteState =
    { id : Int
    , name : Field Text.Error Text
    , address : Field Text.Error Text
    }


type alias Modifiers =
    { setName : String -> State -> State
    , setWebsiteName : ( Int, String ) -> State -> State
    , setWebsiteAddress : ( Int, String ) -> State -> State
    , addWebsite : Int -> State -> State
    , removeWebsite : Int -> State -> State
    }


type Error
    = NameError Text.Error
    | WebsiteNameError Text.Error
    | WebsiteAddressError Text.Error


type alias Output =
    { name : Text
    , websites : List Website
    }


type alias Website =
    { name : Text
    , address : Text
    }


form : Int -> Form
form id =
    Form.new
        { init = init id
        , modifiers = modifiers
        , validate = validate
        }



-- INIT


init : Int -> State
init id =
    { name = Field.empty (Text.fieldType 2)
    , websites =
        [ { id = id
          , name = Field.fromString (Text.fieldType 1) "Elm"
          , address = Field.fromString (Text.fieldType 1) "https://elm-lang.org/"
          }
        ]
    }



-- MODIFIERS


modifiers : Modifiers
modifiers =
    { setName =
        \s state ->
            { state | name = Field.setFromString s state.name }
    , setWebsiteName =
        \( id, s ) state ->
            { state
                | websites =
                    List.map
                        (\website ->
                            if website.id == id then
                                { website | name = Field.setFromString s website.name }

                            else
                                website
                        )
                        state.websites
            }
    , setWebsiteAddress =
        \( id, s ) state ->
            { state
                | websites =
                    List.map
                        (\website ->
                            if website.id == id then
                                { website | address = Field.setFromString s website.address }

                            else
                                website
                        )
                        state.websites
            }
    , addWebsite =
        \id state ->
            { state
                | websites =
                    state.websites
                        ++ [ { id = id
                             , name = Field.empty (Text.fieldType 1)
                             , address = Field.fromString (Text.fieldType 1) "https://"
                             }
                           ]
            }
    , removeWebsite =
        \id state ->
            { state | websites = List.filter (.id >> (/=) id) state.websites }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Field.succeed Output
        |> Field.applyValidation (state.name |> Field.mapError NameError)
        |> V.apply (validateWebsites state.websites)


validateWebsites : List WebsiteState -> Validation Error (List Website)
validateWebsites stateList =
    case stateList of
        [] ->
            V.succeed []

        state :: rest ->
            V.map2
                (::)
                (validateWebsiteState state)
                (validateWebsites rest)


validateWebsiteState : WebsiteState -> Validation Error Website
validateWebsiteState state =
    Field.validate2
        Website
        (state.name |> Field.mapError WebsiteNameError)
        (state.address |> Field.mapError WebsiteAddressError)
