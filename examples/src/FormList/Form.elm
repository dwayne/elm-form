module FormList.Form exposing
    ( Error
    , Fields
    , Form
    , Output
    , Setters
    , Website
    , WebsiteFields
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field)
import Form
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form Fields Setters Error Output


type alias Fields =
    { name : Field Text.Error Text
    , websites : List WebsiteFields
    }


type alias WebsiteFields =
    { id : Int
    , name : Field Text.Error Text
    , address : Field Text.Error Text
    }


type alias Setters =
    { setName : String -> Fields -> Fields
    , setWebsiteName : ( Int, String ) -> Fields -> Fields
    , setWebsiteAddress : ( Int, String ) -> Fields -> Fields
    , addWebsite : Int -> Fields -> Fields
    , removeWebsite : Int -> Fields -> Fields
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
        { setters = setters
        , validate = validate
        }
        { name = Field.empty (Text.fieldType 2)
        , websites =
            [ { id = id
              , name = Field.fromString (Text.fieldType 1) "Elm"
              , address = Field.fromString (Text.fieldType 1) "https://elm-lang.org/"
              }
            ]
        }



-- SETTERS


setters : Setters
setters =
    { setName =
        \s fields ->
            { fields | name = Field.setFromString s fields.name }
    , setWebsiteName =
        \( id, s ) fields ->
            { fields
                | websites =
                    List.map
                        (\website ->
                            if website.id == id then
                                { website | name = Field.setFromString s website.name }

                            else
                                website
                        )
                        fields.websites
            }
    , setWebsiteAddress =
        \( id, s ) fields ->
            { fields
                | websites =
                    List.map
                        (\website ->
                            if website.id == id then
                                { website | address = Field.setFromString s website.address }

                            else
                                website
                        )
                        fields.websites
            }
    , addWebsite =
        \id fields ->
            { fields
                | websites =
                    fields.websites
                        ++ [ { id = id
                             , name = Field.empty (Text.fieldType 1)
                             , address = Field.fromString (Text.fieldType 1) "https://"
                             }
                           ]
            }
    , removeWebsite =
        \id fields ->
            { fields | websites = List.filter (.id >> (/=) id) fields.websites }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    Field.succeed Output
        |> Field.applyValidation (fields.name |> Field.mapError NameError)
        |> V.apply (validateWebsites fields.websites)


validateWebsites : List WebsiteFields -> Validation Error (List Website)
validateWebsites fieldsList =
    case fieldsList of
        [] ->
            V.succeed []

        fields :: rest ->
            V.map2
                (::)
                (validateWebsiteFields fields)
                (validateWebsites rest)


validateWebsiteFields : WebsiteFields -> Validation Error Website
validateWebsiteFields fields =
    Field.validate2
        Website
        (fields.name |> Field.mapError WebsiteNameError)
        (fields.address |> Field.mapError WebsiteAddressError)
