module DynamicForm.Form.Dynamic exposing
    ( Accessors
    , Error
    , Form
    , Output(..)
    , form
    )

import Data.Text as Text exposing (Text)
import DynamicForm.Form.Post as Post
import DynamicForm.Form.Question as Question
import DynamicForm.Publication as Publication exposing (Publication)
import Field.Advanced as Field exposing (Field)
import Form exposing (Accessor)
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form State Accessors Error Output


type alias State =
    { publication : Field Publication.Error Publication
    , post : Post.Form
    , question : Question.Form
    }


type alias Accessors =
    { publication : Accessor State (Field Publication.Error Publication)
    , postBody : Accessor State (Field Text.Error Text)
    , questionTitle : Accessor State (Field Text.Error Text)
    , questionBody : Accessor State (Field Text.Error (Maybe Text))
    }


type Error
    = PublicationError Publication.Error
    | PostError Post.Error
    | QuestionError Question.Error


type Output
    = PostOutput Post.Output
    | QuestionOutput Question.Output


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
    { publication = Field.empty Publication.fieldType
    , post = Post.form
    , question = Question.form
    }



-- ACCESSORS


accessors : Accessors
accessors =
    { publication =
        { get = .publication
        , modify = \f state -> { state | publication = f state.publication }
        }
    , postBody =
        { get = .post >> Form.get .body
        , modify = \f state -> { state | post = Form.modify .body f state.post }
        }
    , questionTitle =
        { get = .question >> Form.get .title
        , modify = \f state -> { state | question = Form.modify .title f state.question }
        }
    , questionBody =
        { get = .question >> Form.get .body
        , modify = \f state -> { state | question = Form.modify .body f state.question }
        }
    }



-- VALIDATE


validate : State -> Validation Error Output
validate state =
    state.publication
        |> Field.mapError PublicationError
        |> Field.validate identity
        |> V.andThen
            (\publication ->
                case publication of
                    Publication.Post ->
                        Form.validate state.post
                            |> V.mapError PostError
                            |> V.map PostOutput

                    Publication.Question ->
                        Form.validate state.question
                            |> V.mapError QuestionError
                            |> V.map QuestionOutput
            )
