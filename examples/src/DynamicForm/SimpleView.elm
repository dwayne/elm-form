module DynamicForm.SimpleView exposing (main)

import Browser as B
import Data.Text as Text
import DynamicForm.Error as Error
import DynamicForm.Form.Dynamic as Dynamic
import DynamicForm.Publication as Publication exposing (Publication)
import Field.Advanced as Field
import Form
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Browser.Dom as BD
import Lib.Bulma.Input
import Lib.Bulma.Select
import Lib.Bulma.Textarea
import Lib.Html.Select as Select


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { dynamic : Dynamic.Form
    , maybeOutput : Maybe Dynamic.Output
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dynamic = Dynamic.form
      , maybeOutput = Nothing
      }
    , focusPublication
    )



-- UPDATE


type Msg
    = Focus
    | InputPublication Publication
    | InputPostBody String
    | InputQuestionTitle String
    | InputQuestionBody String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus ->
            ( model, Cmd.none )

        InputPublication publication ->
            ( { model | dynamic = Form.update .setPublication publication model.dynamic }
            , Cmd.none
            )

        InputPostBody s ->
            ( { model | dynamic = Form.update .setPost ( .setBody, s ) model.dynamic }
            , Cmd.none
            )

        InputQuestionTitle s ->
            ( { model | dynamic = Form.update .setQuestion ( .setTitle, s ) model.dynamic }
            , Cmd.none
            )

        InputQuestionBody s ->
            ( { model | dynamic = Form.update .setQuestion ( .setBody, s ) model.dynamic }
            , Cmd.none
            )

        Submit ->
            ( { model | dynamic = Dynamic.form, maybeOutput = Form.validateAsMaybe model.dynamic }
            , focusPublication
            )


focusPublication : Cmd Msg
focusPublication =
    BD.focus "publication" Focus



-- VIEW


view : Model -> H.Html Msg
view { dynamic, maybeOutput } =
    let
        state =
            Form.toState dynamic
    in
    viewCenter
        [ H.h1 [ HA.class "title is-1" ] [ H.text "Dynamic Form" ]
        , H.form
            [ HA.class "block"
            , HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ Lib.Bulma.Select.view
                { id = "publication"
                , label = "Type of publication"
                , field = state.publication
                , options =
                    ( Select.Label "-- Choose a type --"
                    , [ Publication.Post
                      , Publication.Question
                      ]
                    )
                , optionToString =
                    \publication ->
                        case publication of
                            Publication.Post ->
                                "Post"

                            Publication.Question ->
                                "Question"
                , errorToString = Error.publicationErrorToString
                , isRequired = True
                , isDisabled = False
                , onInput = InputPublication
                , selectAttrs = [ HA.autofocus True ]
                , optionAttrs = always []
                }
            , case Field.toMaybe state.publication of
                Just Publication.Post ->
                    let
                        postFields =
                            Form.toState state.post
                    in
                    H.fieldset [ HA.class "block" ]
                        [ Lib.Bulma.Textarea.view
                            { id = "post-body"
                            , label = "Body"
                            , field = postFields.body
                            , errorToString = Error.textErrorToString
                            , isRequired = True
                            , isDisabled = False
                            , attrs = [ HA.placeholder "Type your post here..." ]
                            , onInput = InputPostBody
                            }
                        ]

                Just Publication.Question ->
                    let
                        questionFields =
                            Form.toState state.question
                    in
                    H.fieldset [ HA.class "block" ]
                        [ Lib.Bulma.Input.view
                            { id = "question-title"
                            , label = "Title"
                            , tipe = Lib.Bulma.Input.Text
                            , field = questionFields.title
                            , errorToString = Error.textErrorToString
                            , isRequired = True
                            , isDisabled = False
                            , onInput = InputQuestionTitle
                            , attrs = [ HA.placeholder "Type your question here..." ]
                            }
                        , Lib.Bulma.Textarea.view
                            { id = "question-body"
                            , label = "Body"
                            , field = questionFields.body
                            , errorToString = Error.textErrorToString
                            , isRequired = True
                            , isDisabled = False
                            , attrs = [ HA.placeholder "Describe your question here... (optional)" ]
                            , onInput = InputQuestionBody
                            }
                        ]

                Nothing ->
                    H.text ""
            , H.div [ HA.class "field" ]
                [ H.div [ HA.class "control" ]
                    [ H.button
                        [ HA.class "button is-link"
                        , HA.disabled <| Form.isInvalid dynamic
                        ]
                        [ H.text "New Publication" ]
                    ]
                ]
            ]
        , case maybeOutput of
            Just (Dynamic.PostOutput { body }) ->
                H.div [ HA.class "content" ]
                    [ H.h2 [ HA.class "title is-2" ] [ H.text "Post Output" ]
                    , H.p []
                        [ H.strong [] [ H.text "Body:" ]
                        , H.text " "
                        , H.text (Text.toString body)
                        ]
                    ]

            Just (Dynamic.QuestionOutput { title, body }) ->
                H.div [ HA.class "content" ]
                    [ H.h2 [ HA.class "title is-2" ] [ H.text "Question Output" ]
                    , H.p []
                        [ H.strong [] [ H.text "Title:" ]
                        , H.text " "
                        , H.text (Text.toString title)
                        ]
                    , H.p []
                        [ H.strong [] [ H.text "Body:" ]
                        , H.text " "
                        , body
                            |> Maybe.map Text.toString
                            |> Maybe.withDefault ""
                            |> H.text
                        ]
                    ]

            Nothing ->
                H.text ""
        ]


viewCenter : List (H.Html msg) -> H.Html msg
viewCenter children =
    H.div [ HA.class "container p-4" ]
        [ H.div [ HA.class "columns" ]
            [ H.div [ HA.class "column is-half is-offset-one-quarter" ] children
            ]
        ]
