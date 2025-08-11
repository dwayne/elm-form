module Test.DynamicForm.Form.Dynamic exposing (suite)

import DynamicForm.Form.Dynamic as Dynamic
import DynamicForm.Publication as Publication
import DynamicForm.Text as Text
import Expect
import Field.Advanced as Field
import Form
import Fuzz
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "SignUp.Form"
        [ describe "initial state"
            [ test "it is invalid" <|
                \_ ->
                    Dynamic.form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "publication is blank" <|
                \_ ->
                    Dynamic.form
                        |> Form.toFields
                        |> .publication
                        |> Field.allErrors
                        |> Expect.equal [ Field.blankError ]
            ]
        , describe "when publication is Post" <|
            let
                form =
                    Dynamic.form
                        |> Form.update .setPublication Publication.Post

                post =
                    form
                        |> Form.toFields
                        |> .post
            in
            [ test "it is invalid" <|
                \_ ->
                    form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "when post body is blank" <|
                \_ ->
                    form
                        |> Form.toFields
                        |> .post
                        |> Form.toFields
                        |> .body
                        |> Field.allErrors
                        |> Expect.equal [ Field.blankError ]
            , test "when post body has less than 10 characters" <|
                \_ ->
                    form
                        |> Form.update .setPost (Form.update .setBody "Hello" post)
                        |> Form.toFields
                        |> .post
                        |> Form.toFields
                        |> .body
                        |> Field.allErrors
                        |> Expect.equal [ Field.customError (Text.TooShort 10) ]
            , fuzz (Fuzz.oneOfValues [ String.repeat 10 "a", String.repeat 10 "ab" ]) "when post body has 10 characters or more" <|
                \b ->
                    form
                        |> Form.update .setPost (Form.update .setBody b post)
                        |> Form.validateAsMaybe
                        |> Maybe.map
                            (\output ->
                                case output of
                                    Dynamic.PostOutput { body } ->
                                        { body = Text.toString body
                                        }

                                    _ ->
                                        { body = ""
                                        }
                            )
                        |> Expect.equal
                            (Just
                                { body = b }
                            )
            ]
        , describe "when publication is Question" <|
            let
                form =
                    Dynamic.form
                        |> Form.update .setPublication Publication.Question

                question =
                    form
                        |> Form.toFields
                        |> .question
            in
            [ test "it is invalid" <|
                \_ ->
                    form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "when question title is blank" <|
                \_ ->
                    form
                        |> Form.toFields
                        |> .question
                        |> Form.toFields
                        |> .title
                        |> Field.allErrors
                        |> Expect.equal [ Field.blankError ]
            , test "when question body is blank" <|
                \_ ->
                    form
                        |> Form.toFields
                        |> .question
                        |> Form.toFields
                        |> .body
                        |> Field.allErrors
                        |> Expect.equal [ Field.blankError ]
            , test "when question title has less than 10 characters" <|
                \_ ->
                    form
                        |> Form.update .setQuestion (Form.update .setTitle "A title" question)
                        |> Form.toFields
                        |> .question
                        |> Form.toFields
                        |> .title
                        |> Field.allErrors
                        |> Expect.equal [ Field.customError (Text.TooShort 10) ]
            , test "when question body has less than 100 characters" <|
                \_ ->
                    form
                        |> Form.update .setQuestion (Form.update .setBody "A body" question)
                        |> Form.toFields
                        |> .question
                        |> Form.toFields
                        |> .body
                        |> Field.allErrors
                        |> Expect.equal [ Field.customError (Text.TooShort 100) ]
            , fuzz2
                (Fuzz.oneOfValues [ String.repeat 10 "a", String.repeat 10 "ab" ])
                (Fuzz.oneOfValues [ String.repeat 50 "ab", String.repeat 50 "abcd" ])
                "when question title has 10 characters or more and question body has 100 characters or more"
              <|
                \t b ->
                    form
                        |> Form.update .setQuestion (Form.update .setTitle t question)
                        |> (\newForm ->
                                let
                                    newQuestion =
                                        newForm
                                            |> Form.toFields
                                            |> .question
                                in
                                newForm
                                    |> Form.update .setQuestion (Form.update .setBody b newQuestion)
                           )
                        |> Form.validateAsMaybe
                        |> Maybe.map
                            (\output ->
                                case output of
                                    Dynamic.QuestionOutput { title, body } ->
                                        { title = Text.toString title
                                        , body = Text.toString body
                                        }

                                    _ ->
                                        { title = ""
                                        , body = ""
                                        }
                            )
                        |> Expect.equal
                            (Just
                                { title = t
                                , body = b
                                }
                            )
            ]
        ]
