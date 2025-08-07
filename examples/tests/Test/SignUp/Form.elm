module Test.SignUp.Form exposing (suite)

import Expect
import Field.Advanced as Field
import Form
import SignUp.Email as Email
import SignUp.Form as SignUp
import SignUp.Username as Username
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SignUp.Form"
        [ describe "initial state"
            [ test "it is invalid" <|
                \_ ->
                    SignUp.form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "username is empty" <|
                \_ ->
                    SignUp.form
                        |> Form.toFields
                        |> .username
                        |> Field.toRawString
                        |> String.isEmpty
                        |> Expect.equal True
            , test "email is empty" <|
                \_ ->
                    SignUp.form
                        |> Form.toFields
                        |> .email
                        |> Field.toRawString
                        |> String.isEmpty
                        |> Expect.equal True
            ]
        , describe "with valid data" <|
            let
                validForm =
                    SignUp.form
                        |> Form.update .setUsername "freddy"
                        |> Form.update .setEmail "freddy.mercury@queen.com"
            in
            [ test "it is valid" <|
                \_ ->
                    validForm
                        |> Form.isValid
                        |> Expect.equal True
            , test "output" <|
                \_ ->
                    validForm
                        |> Form.validateAsMaybe
                        |> Maybe.map
                            (\{ username, email } ->
                                { username = Username.toString username
                                , email = Email.toString email
                                }
                            )
                        |> Expect.equal
                            (Just
                                { username = "freddy"
                                , email = "freddy.mercury@queen.com"
                                }
                            )
            ]
        ]
