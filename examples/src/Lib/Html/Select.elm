module Lib.Html.Select exposing
    ( Default(..)
    , ViewOptions
    , view
    )

import Field.Advanced as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD


type alias ViewOptions e a msg =
    { field : Field e a
    , options : ( Default a, List a )
    , optionToString : a -> String
    , errorToString : e -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : a -> msg
    , attrs : List (H.Attribute msg)
    , optionAttrs : a -> List (H.Attribute msg)
    }


type Default a
    = Label String
    | Option a


view : ViewOptions e a msg -> H.Html msg
view { field, options, optionToString, errorToString, isRequired, isDisabled, onInput, attrs, optionAttrs } =
    let
        requiredAttrs =
            if isRequired then
                [ HA.required True
                ]

            else
                []

        dataEmptyAttrs =
            if F.isEmpty field then
                [ HA.attribute "data-select-empty" ""
                ]

            else
                []

        otherAttrs =
            [ if isDisabled then
                HA.disabled True

              else
                onSelect field errorToString onInput
            , HA.attribute "data-select-state" <|
                if F.isClean field then
                    "clean"

                else
                    "dirty"
            , HA.attribute "data-select-validity" <|
                if F.isValid field then
                    "valid"

                else
                    "invalid"
            ]

        maybeValue =
            F.toMaybe field

        { toString } =
            F.toConverters field

        ( default, restOptions ) =
            options

        ( x, y ) =
            case default of
                Label text ->
                    ( [ H.option
                            [ HA.disabled True
                            , HA.selected (Nothing == maybeValue)
                            ]
                            [ H.text text ]
                      ]
                    , restOptions
                    )

                Option option ->
                    ( []
                    , option :: restOptions
                    )

        htmlOptions =
            x
                ++ List.map
                    (\current ->
                        let
                            isSelected =
                                Just current == maybeValue
                        in
                        if isSelected then
                            H.option
                                (optionAttrs current
                                    ++ [ HA.value (toString current)
                                       , HA.selected True
                                       ]
                                )
                                [ H.text (optionToString current) ]

                        else
                            H.option
                                (optionAttrs current
                                    ++ [ HA.value (toString current)
                                       ]
                                )
                                [ H.text (optionToString current) ]
                    )
                    y
    in
    H.select
        (attrs ++ requiredAttrs ++ dataEmptyAttrs ++ otherAttrs)
        htmlOptions


onSelect : Field e a -> (e -> String) -> (a -> msg) -> H.Attribute msg
onSelect field errorToString toMsg =
    let
        { fromString } =
            F.toConverters field
    in
    HE.stopPropagationOn "input"
        (HE.targetValue
            |> JD.andThen
                (\s ->
                    case fromString s of
                        Ok x ->
                            JD.succeed ( toMsg x, True )

                        Err err ->
                            JD.fail (errorToString err)
                )
        )
