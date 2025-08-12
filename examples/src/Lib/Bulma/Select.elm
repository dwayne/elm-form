module Lib.Bulma.Select exposing (ViewOptions, view)

import Field.Advanced as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Lib.Html.Select as Select


type alias ViewOptions e a msg =
    { id : String
    , label : String
    , field : Field e a
    , options : ( Select.Default a, List a )
    , optionToString : a -> String
    , errorToString : e -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : a -> msg
    , selectAttrs : List (H.Attribute msg)
    , optionAttrs : a -> List (H.Attribute msg)
    }


view : ViewOptions e a msg -> H.Html msg
view { id, label, field, options, optionToString, errorToString, isRequired, isDisabled, onInput, selectAttrs, optionAttrs } =
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
            [ H.div [ HA.class "select" ]
                [ Select.view
                    { field = field
                    , options = options
                    , optionToString = optionToString
                    , errorToString = errorToString
                    , isRequired = isRequired
                    , isDisabled = isDisabled
                    , onInput = onInput
                    , attrs =
                        selectAttrs
                            ++ [ HA.classList
                                    [ ( "is-success", isDirty && isValid )
                                    , ( "is-danger", isDirty && not isValid )
                                    ]
                               , HA.id id
                               ]
                    , optionAttrs = optionAttrs
                    }
                ]
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
