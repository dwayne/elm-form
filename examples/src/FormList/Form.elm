module FormList.Form exposing
    ( Accessors
    , Error
    , Form
    , Output
    , State
    , form
    )

import Data.Text as Text exposing (Text)
import Field.Advanced as Field exposing (Field)
import Form exposing (Accessor)
import Form.List exposing (Forms)
import FormList.Website as Website
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { name : Field Text.Error Text
    , websites : Forms Website.Form
    }


type alias Accessors =
    { name : Accessor State (Field Text.Error Text)
    , websiteName : Int -> Accessor State (Field Text.Error Text)
    , websiteAddress : Int -> Accessor State (Field Text.Error Text)
    , addWebsite : State -> State
    , removeWebsite : Int -> State -> State
    }


type Error
    = NameError Text.Error
    | WebsiteError Website.Error


type alias Output =
    { name : Text
    , websites : List Website.Output
    }


form : Form
form =
    Form.new
        { init = init
        , accessors = accessors
        , validate = validate
        }



-- INIT


init : State
init =
    { name = Field.empty (Text.fieldType 2)
    , websites = Form.List.fromList [ Website.form "Elm" "https://elm-lang.org/" ]
    }



-- ACCESSORS


accessors : Accessors
accessors =
    { name =
        { get = .name
        , modify = \f state -> { state | name = f state.name }
        }
    , websiteName =
        \id ->
            { get = .websites >> Form.List.get id .name emptyWebsiteName
            , modify = \f state -> { state | websites = Form.List.modify id .name f state.websites }
            }
    , websiteAddress =
        \id ->
            { get = .websites >> Form.List.get id .address emptyWebsiteAddress
            , modify = \f state -> { state | websites = Form.List.modify id .address f state.websites }
            }
    , addWebsite = \state -> { state | websites = Form.List.append (Website.form "" "https://") state.websites }
    , removeWebsite = \id state -> { state | websites = Form.List.remove id state.websites }
    }


emptyWebsiteName : Field Text.Error Text
emptyWebsiteName =
    Field.empty (Text.fieldType 1)


emptyWebsiteAddress : Field Text.Error Text
emptyWebsiteAddress =
    Field.empty (Text.fieldType 1)



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Field.succeed Output
        |> Field.applyValidation (state.name |> Field.mapError NameError)
        |> V.apply (Form.List.validate WebsiteError state.websites)
