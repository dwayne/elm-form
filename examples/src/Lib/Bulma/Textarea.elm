module Lib.Bulma.Textarea exposing (ViewOptions, view)

import Field.Advanced as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Lib.Html.Textarea as Textarea


type alias ViewOptions e a msg =
    { id : String
    , label : String
    , field : Field e a
    , errorToString : e -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : String -> msg
    , attrs : List (H.Attribute msg)
    }


view : ViewOptions e a msg -> H.Html msg
view { id, label, field, errorToString, isRequired, isDisabled, onInput, attrs } =
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
            [ Textarea.view
                { field = field
                , isRequired = isRequired
                , isDisabled = isDisabled
                , onInput = onInput
                , attrs =
                    attrs
                        ++ [ HA.class "textarea"
                           , HA.classList
                                [ ( "is-success", isDirty && isValid )
                                , ( "is-danger", isDirty && not isValid )
                                ]
                           , HA.id id
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
