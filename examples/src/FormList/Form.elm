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
import Form3 as Form exposing (Accessor)
import FormList.Website as Website
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { id : Int
    , name : Field Text.Error Text
    , websites : List ( Int, Website.Form )
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
    { id = 1
    , name = Field.empty (Text.fieldType 2)
    , websites = [ ( 0, Website.form "Elm" "https://elm-lang.org/" ) ]
    }


emptyWebsiteName : Field Text.Error Text
emptyWebsiteName =
    Field.empty (Text.fieldType 1)


emptyWebsiteAddress : Field Text.Error Text
emptyWebsiteAddress =
    Field.empty (Text.fieldType 1)



-- ACCESSORS


accessors : Accessors
accessors =
    { name =
        { get = .name
        , modify = \f state -> { state | name = f state.name }
        }
    , websiteName =
        \id ->
            { get =
                .websites
                    >> List.filter (Tuple.first >> (==) id)
                    >> List.head
                    >> Maybe.map (Tuple.second >> Form.get .name)
                    >> Maybe.withDefault emptyWebsiteName
            , modify =
                \f state ->
                    { state
                        | websites =
                            List.map
                                (\( currentId, website ) ->
                                    ( currentId
                                    , if currentId == id then
                                        Form.modify .name f website

                                      else
                                        website
                                    )
                                )
                                state.websites
                    }
            }
    , websiteAddress =
        \id ->
            { get =
                .websites
                    >> List.filter (Tuple.first >> (==) id)
                    >> List.head
                    >> Maybe.map (Tuple.second >> Form.get .address)
                    >> Maybe.withDefault emptyWebsiteAddress
            , modify =
                \f state ->
                    { state
                        | websites =
                            List.map
                                (\( currentId, website ) ->
                                    ( currentId
                                    , if currentId == id then
                                        Form.modify .address f website

                                      else
                                        website
                                    )
                                )
                                state.websites
                    }
            }
    , addWebsite = \state -> { state | id = state.id + 1, websites = state.websites ++ [ ( state.id, Website.form "" "https://" ) ] }
    , removeWebsite = \id state -> { state | websites = List.filter (Tuple.first >> (/=) id) state.websites }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    Field.succeed Output
        |> Field.applyValidation (state.name |> Field.mapError NameError)
        |> V.apply (validateWebsites state.websites)


validateWebsites : List ( Int, Website.Form ) -> Validation Error (List Website.Output)
validateWebsites =
    List.foldr
        (\( _, website ) -> V.map2 (::) (Form.validate website |> V.mapError WebsiteError))
        (V.succeed [])
