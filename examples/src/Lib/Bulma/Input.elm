module Lib.Bulma.Input exposing (Type(..), ViewOptions, view)

import Field.Advanced as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Lib.Html.Input as Input


type alias ViewOptions e a msg =
    { id : String
    , label : String
    , tipe : Type
    , field : Field e a
    , errorToString : e -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : String -> msg
    , attrs : List (H.Attribute msg)
    }


type Type
    = Text
    | Password
    | Email


view : ViewOptions e a msg -> H.Html msg
view { id, label, tipe, field, errorToString, isRequired, isDisabled, onInput, attrs } =
    let
        isDirty =
            F.isDirty field

        isValid =
            F.isValid field
    in
    H.div [ HA.class "field" ]
        [ H.label
            [ HA.class "label"
            , HA.for id
            ]
            [ H.text label ]
        , H.div [ HA.class "control" ]
            [ Input.view
                { field = field
                , isRequired = isRequired
                , isDisabled = isDisabled
                , onInput = onInput
                , attrs =
                    attrs
                        ++ [ HA.class "input"
                           , HA.classList
                                [ ( "is-success", isDirty && isValid )
                                , ( "is-danger", isDirty && not isValid )
                                ]
                           , HA.id id
                           , HA.type_ <|
                                case tipe of
                                    Text ->
                                        "text"

                                    Password ->
                                        "password"

                                    Email ->
                                        "email"
                           ]
                }
            ]
        , if isDirty then
            case F.firstError field of
                Just error ->
                    H.p [ HA.class "help is-danger" ] [ H.text (errorToString error) ]

                Nothing ->
                    H.text ""

          else
            H.text ""
        ]
