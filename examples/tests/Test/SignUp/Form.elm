module Test.SignUp.Form exposing (suite)

import Expect
import Field.Advanced as Field
import Form
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
            ]
        , describe "with valid data" <|
            let
                validForm =
                    SignUp.form
                        |> Form.update .setUsername "freddy"
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
                            (\{ username } ->
                                { username = Username.toString username
                                }
                            )
                        |> Expect.equal
                            (Just
                                { username = "freddy"
                                }
                            )
            ]
        ]
