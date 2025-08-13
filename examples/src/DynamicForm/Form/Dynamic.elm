module DynamicForm.Form.Dynamic exposing
    ( Error
    , Fields
    , Form
    , Output(..)
    , Setters
    , form
    )

import Data.Text as Text exposing (Text)
import DynamicForm.Form.Post as Post
import DynamicForm.Form.Question as Question
import DynamicForm.Publication as Publication exposing (Publication)
import Field.Advanced as Field exposing (Field, Validation)
import Form
import Validation as V



-- FORM


type alias Form =
    Form.Form Fields Setters Error Output


type alias Fields =
    { publication : Field Publication.Error Publication
    , post : Post.Form
    , question : Question.Form
    }


type alias Setters =
    { setPublication : Publication -> Fields -> Fields
    , setPost : Post.Form -> Fields -> Fields
    , setQuestion : Question.Form -> Fields -> Fields
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
        { setters = setters
        , validate = validate
        }
        { publication = Field.empty Publication.fieldType
        , post = Post.form
        , question = Question.form
        }



-- SETTERS


setters : Setters
setters =
    { setPublication =
        \publication fields ->
            { fields | publication = Field.setFromValue publication fields.publication }
    , setPost =
        \post fields ->
            { fields | post = post }
    , setQuestion =
        \question fields ->
            { fields | question = question }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    fields.publication
        |> Field.mapError PublicationError
        |> Field.validate identity
        |> V.andThen
            (\publication ->
                case publication of
                    Publication.Post ->
                        Form.validate fields.post
                            |> V.mapError PostError
                            |> V.map PostOutput

                    Publication.Question ->
                        Form.validate fields.question
                            |> V.mapError QuestionError
                            |> V.map QuestionOutput
            )
