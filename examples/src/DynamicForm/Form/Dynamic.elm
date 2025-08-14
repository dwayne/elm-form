module DynamicForm.Form.Dynamic exposing
    ( Error
    , Form
    , Modifiers
    , Output(..)
    , State
    , form
    )

import Data.Text as Text exposing (Text)
import DynamicForm.Form.Post as Post
import DynamicForm.Form.Question as Question
import DynamicForm.Publication as Publication exposing (Publication)
import Field.Advanced as Field exposing (Field)
import Form
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form State Modifiers Error Output


type alias State =
    { publication : Field Publication.Error Publication
    , post : Post.Form
    , question : Question.Form
    }


type alias Modifiers =
    { setPublication : Publication -> State -> State
    , setPost : Post.Form -> State -> State
    , setQuestion : Question.Form -> State -> State
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
        , modifiers = modifiers
        , validate = validate
        }



-- INIT


init : State
init =
    { publication = Field.empty Publication.fieldType
    , post = Post.form
    , question = Question.form
    }



-- MODIFIERS


modifiers : Modifiers
modifiers =
    { setPublication =
        \publication state ->
            { state | publication = Field.setFromValue publication state.publication }
    , setPost =
        \post state ->
            { state | post = post }
    , setQuestion =
        \question state ->
            { state | question = question }
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
