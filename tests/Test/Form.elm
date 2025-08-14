module Test.Form exposing (suite)

import Expect
import Field
import Form
import Test exposing (Test, describe, test)
import Test.Fixtures.Form.Name as Name


suite : Test
suite =
    describe "Form"
        [ nameFormSuite
        ]


nameFormSuite : Test
nameFormSuite =
    describe "Name Form"
        [ describe "initial state"
            [ test "it is invalid" <|
                \_ ->
                    Name.form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "firstName is empty" <|
                \_ ->
                    Name.form
                        |> Form.toState
                        |> .firstName
                        |> Field.isEmpty
                        |> Expect.equal True
            , test "firstName is invalid" <|
                \_ ->
                    Name.form
                        |> Form.toState
                        |> .firstName
                        |> Field.isInvalid
                        |> Expect.equal True
            , test "lastName is empty" <|
                \_ ->
                    Name.form
                        |> Form.toState
                        |> .lastName
                        |> Field.isEmpty
                        |> Expect.equal True
            , test "lastName is valid" <|
                \_ ->
                    Name.form
                        |> Form.toState
                        |> .lastName
                        |> Field.isValid
                        |> Expect.equal True
            ]
        , describe "when blank first name and blank last name" <|
            [ test "it is invalid" <|
                \_ ->
                    Name.form
                        |> Form.update .setFirstName "   "
                        |> Form.update .setLastName " \t "
                        |> Form.validateAsResult
                        |> Expect.equal (Err [ Name.FirstNameError Field.blankError ])
            ]
        , describe "with non-blank first name and blank last name"
            [ test "it is valid with the full name being the first name" <|
                \_ ->
                    Name.form
                        |> Form.update .setFirstName "Dave"
                        |> Form.update .setLastName " \t "
                        |> Form.validateAsMaybe
                        |> Expect.equal (Just "Dave")
            ]
        , describe "when non-blank first name and non-blank last name"
            [ test "it is valid with the full name being the first and last name" <|
                \_ ->
                    Name.form
                        |> Form.update .setFirstName "Dave"
                        |> Form.update .setLastName "MacQueen"
                        |> Form.validateAsMaybe
                        |> Expect.equal (Just "Dave MacQueen")
            ]
        ]
