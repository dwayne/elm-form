module FormList.SimpleView exposing (main)

import Browser as B
import Data.Text as Text
import Field.Advanced as Field
import Form
import FormList.Error as Error
import FormList.Form as FormList
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Browser.Dom as BD
import Lib.Bulma.Input
import Lib.Bulma.Select
import Lib.Bulma.Textarea
import Lib.Html.Select as Select


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { formList : FormList.Form
    , maybeOutput : Maybe FormList.Output
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { formList = FormList.form
      , maybeOutput = Nothing
      }
    , focusName
    )



-- UPDATE


type Msg
    = Focus
    | InputName String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus ->
            ( model, Cmd.none )

        InputName s ->
            ( { model | formList = Form.update .setName s model.formList }
            , Cmd.none
            )

        Submit ->
            ( { model | formList = FormList.form, maybeOutput = Form.validateAsMaybe model.formList }
            , focusName
            )


focusName : Cmd Msg
focusName =
    BD.focus "name" Focus



-- VIEW


view : Model -> H.Html Msg
view { formList, maybeOutput } =
    let
        fields =
            Form.toFields formList
    in
    viewCenter
        [ H.h1 [ HA.class "title is-1" ] [ H.text "Form list" ]
        , H.form
            [ HA.class "block"
            , HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ Lib.Bulma.Input.view
                { id = "name"
                , label = "Your name"
                , tipe = Lib.Bulma.Input.Text
                , field = fields.name
                , errorToString = Error.textErrorToString
                , isRequired = True
                , isDisabled = False
                , onInput = InputName
                , attrs = [ HA.placeholder "Type your name" ]
                }
            , H.div [ HA.class "field" ]
                [ H.div [ HA.class "control" ]
                    [ H.button
                        [ HA.class "button is-link"
                        , HA.disabled <| Form.isInvalid formList
                        ]
                        [ H.text "Submit" ]
                    ]
                ]
            ]
        , case maybeOutput of
            Just { name } ->
                H.div [ HA.class "content" ]
                    [ H.h2 [ HA.class "title is-2" ] [ H.text "Output" ]
                    , H.p []
                        [ H.strong [] [ H.text "Name:" ]
                        , H.text " "
                        , H.text (Text.toString name)
                        ]
                    ]

            Nothing ->
                H.text ""
        ]


viewCenter : List (H.Html msg) -> H.Html msg
viewCenter children =
    H.div [ HA.class "container p-4" ]
        [ H.div [ HA.class "columns" ]
            [ H.div [ HA.class "column is-half is-offset-one-quarter" ] children
            ]
        ]
