module Test.Form.List exposing (suite)

import Expect
import Field
import Form
import Form.List
import Test exposing (Test, describe, test)
import Test.Fixtures.Form.Group as Group


suite : Test
suite =
    describe "Form.List"
        [ groupFormSuite
        ]


groupFormSuite : Test
groupFormSuite =
    describe "Group Form"
        [ describe "initial state"
            [ test "it is invalid" <|
                \_ ->
                    Group.form
                        |> Form.isInvalid
                        |> Expect.equal True
            , test "name is empty" <|
                \_ ->
                    Group.form
                        |> Form.get .name
                        |> Field.isEmpty
                        |> Expect.equal True
            , test "name is invalid" <|
                \_ ->
                    Group.form
                        |> Form.get .name
                        |> Field.isInvalid
                        |> Expect.equal True
            , test "description is empty" <|
                \_ ->
                    Group.form
                        |> Form.get .description
                        |> Field.isEmpty
                        |> Expect.equal True
            , test "description is invalid" <|
                \_ ->
                    Group.form
                        |> Form.get .description
                        |> Field.isInvalid
                        |> Expect.equal True
            , test "people is empty" <|
                \_ ->
                    Group.form
                        |> Form.get .people
                        |> Form.List.toList
                        |> List.isEmpty
                        |> Expect.equal True
            ]
        , describe "when both name and description are non-blank" <|
            let
                --
                -- Based on the UChicago Programming Languages Group:
                --
                -- https://cs.uchicago.edu/research/programming-languages/
                --
                name =
                    "Programming Languages Group"

                description =
                    "Interested in all aspects of programming language design and implementation, ranging from theoretical foundations to practical applications."

                group =
                    Group.form
                        |> Form.modify .name (Field.setFromString name)
                        |> Form.modify .description (Field.setFromString description)
            in
            [ test "it is valid" <|
                \_ ->
                    group
                        |> Form.isValid
                        |> Expect.equal True
            , test "name is valid" <|
                \_ ->
                    group
                        |> Form.get .name
                        |> Field.toMaybe
                        |> Expect.equal (Just name)
            , test "description is valid" <|
                \_ ->
                    group
                        |> Form.get .description
                        |> Field.toMaybe
                        |> Expect.equal (Just description)
            , describe "when people are added" <|
                let
                    groupWithPeople =
                        group
                            |> Form.update .addPerson
                            |> Form.update .addPerson
                            |> Form.update .addPerson
                            |> Form.modify .people
                                (Form.List.modify 0 .firstName (Field.setFromString "Ravi")
                                    >> Form.List.modify 0 .lastName (Field.setFromString "Chugh")
                                )
                            |> Form.modify .people
                                (Form.List.modify 1 .firstName (Field.setFromString "Robert")
                                    >> Form.List.modify 1 .lastName (Field.setFromString "Rand")
                                )
                            |> Form.modify .people
                                (Form.List.modify 2 .firstName (Field.setFromString "John")
                                    >> Form.List.modify 2 .lastName (Field.setFromString "Reppy")
                                )
                in
                [ test "it is valid" <|
                    \_ ->
                        groupWithPeople
                            |> Form.validateAsMaybe
                            |> Expect.equal
                                (Just
                                    { name = name
                                    , description = description
                                    , people =
                                        [ "Ravi Chugh"
                                        , "Robert Rand"
                                        , "John Reppy"
                                        ]
                                    }
                                )
                , test "the 2nd person is removed" <|
                    \_ ->
                        groupWithPeople
                            |> Form.update (\r -> r.removePerson 1)
                            |> Form.validateAsMaybe
                            |> Expect.equal
                                (Just
                                    { name = name
                                    , description = description
                                    , people =
                                        [ "Ravi Chugh"
                                        , "John Reppy"
                                        ]
                                    }
                                )
                ]
            ]
        ]
