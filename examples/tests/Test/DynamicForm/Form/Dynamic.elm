module Test.DynamicForm.Form.Dynamic exposing (suite)

import Data.Text as Text
import DynamicForm.Form.Dynamic as Dynamic
import DynamicForm.Publication as Publication
import Expect
import Field.Advanced as Field
import Form3 as Form
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
                        |> Form.get .publication
                        |> Field.allErrors
                        |> Expect.equal [ Field.blankError ]
            ]
        , describe "when publication is Post" <|
            let
                form =
                    Dynamic.form
                        |> Form.modify .publication (Field.setFromValue Publication.Post)
            in
            [ test "it is invalid" <|
                \_ ->
                    form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "when post body is blank" <|
                \_ ->
                    form
                        |> Form.get .postBody
                        |> Field.allErrors
                        |> Expect.equal [ Field.blankError ]
            , test "when post body has less than 10 characters" <|
                \_ ->
                    form
                        |> Form.modify .postBody (Field.setFromString "Hello")
                        |> Form.get .postBody
                        |> Field.allErrors
                        |> Expect.equal [ Field.customError (Text.TooShort 10) ]
            , fuzz (Fuzz.oneOfValues [ String.repeat 10 "a", String.repeat 10 "ab" ]) "when post body has 10 characters or more" <|
                \b ->
                    form
                        |> Form.modify .postBody (Field.setFromString b)
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
                        |> Form.modify .publication (Field.setFromValue Publication.Question)
            in
            [ test "it is invalid" <|
                \_ ->
                    form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "when question title is blank" <|
                \_ ->
                    form
                        |> Form.get .questionTitle
                        |> Field.allErrors
                        |> Expect.equal [ Field.blankError ]
            , test "when question body is blank" <|
                \_ ->
                    form
                        |> Form.get .questionBody
                        |> Field.allErrors
                        |> Expect.equal []
            , test "when question title has less than 10 characters" <|
                \_ ->
                    form
                        |> Form.modify .questionTitle (Field.setFromString "A title")
                        |> Form.get .questionTitle
                        |> Field.allErrors
                        |> Expect.equal [ Field.customError (Text.TooShort 10) ]
            , test "when question body has less than 100 characters" <|
                \_ ->
                    form
                        |> Form.modify .questionBody (Field.setFromString "A body")
                        |> Form.get .questionBody
                        |> Field.allErrors
                        |> Expect.equal [ Field.customError (Text.TooShort 100) ]
            , fuzz (Fuzz.oneOfValues [ String.repeat 10 "a", String.repeat 10 "ab" ]) "when question title has 10 characters or more" <|
                \t ->
                    form
                        |> Form.modify .questionTitle (Field.setFromString t)
                        |> Form.validateAsMaybe
                        |> Maybe.map
                            (\output ->
                                case output of
                                    Dynamic.QuestionOutput { title, body } ->
                                        { title = Text.toString title
                                        , body = body |> Maybe.map Text.toString |> Maybe.withDefault ""
                                        }

                                    _ ->
                                        { title = ""
                                        , body = ""
                                        }
                            )
                        |> Expect.equal
                            (Just
                                { title = t
                                , body = ""
                                }
                            )
            , fuzz2
                (Fuzz.oneOfValues [ String.repeat 10 "a", String.repeat 10 "ab" ])
                (Fuzz.oneOfValues [ String.repeat 50 "ab", String.repeat 50 "abcd" ])
                "when question title has 10 characters or more and question body has 100 characters or more"
              <|
                \t b ->
                    form
                        |> Form.modify .questionTitle (Field.setFromString t)
                        |> Form.modify .questionBody (Field.setFromString b)
                        |> Form.validateAsMaybe
                        |> Maybe.map
                            (\output ->
                                case output of
                                    Dynamic.QuestionOutput { title, body } ->
                                        { title = Text.toString title
                                        , body = body |> Maybe.map Text.toString |> Maybe.withDefault ""
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
