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
import Html.Keyed as HK
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
    { id : Int
    , formList : FormList.Form
    , maybeOutput : Maybe FormList.Output
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { id = 1
      , formList = FormList.form 0
      , maybeOutput = Nothing
      }
    , focusName
    )



-- UPDATE


type Msg
    = Focus
    | InputName String
    | InputWebsiteName Int String
    | InputWebsiteAddress Int String
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

        InputWebsiteName id s ->
            ( { model | formList = Form.update .setWebsiteName ( id, s ) model.formList }
            , Cmd.none
            )

        InputWebsiteAddress id s ->
            ( { model | formList = Form.update .setWebsiteAddress ( id, s ) model.formList }
            , Cmd.none
            )

        Submit ->
            ( { model
                | id = model.id + 1
                , formList = FormList.form model.id
                , maybeOutput = Form.validateAsMaybe model.formList
              }
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
                [ H.strong [] [ H.text "Websites" ]
                ]
            , HK.node "div" [ HA.class "field" ] <|
                List.indexedMap
                    (\index website ->
                        let
                            id =
                                String.fromInt website.id
                        in
                        ( id
                        , H.div [ HA.class "box" ]
                            [ H.button
                                [ HA.class "delete"
                                , HA.type_ "button"
                                ]
                                []
                            , Lib.Bulma.Input.view
                                { id = "website-name-" ++ id
                                , label = "Name of website #" ++ String.fromInt (index + 1)
                                , tipe = Lib.Bulma.Input.Text
                                , field = website.name
                                , errorToString = Error.textErrorToString
                                , isRequired = True
                                , isDisabled = False
                                , onInput = InputWebsiteName website.id
                                , attrs = []
                                }
                            , Lib.Bulma.Input.view
                                { id = "website-address-" ++ id
                                , label = "Address of website #" ++ String.fromInt (index + 1)
                                , tipe = Lib.Bulma.Input.Text
                                , field = website.address
                                , errorToString = Error.textErrorToString
                                , isRequired = True
                                , isDisabled = False
                                , onInput = InputWebsiteAddress website.id
                                , attrs = [ HA.placeholder "https://..." ]
                                }
                            ]
                        )
                    )
                    fields.websites
            , H.div [ HA.class "field" ]
                [ H.button
                    [ HA.class "button is-text"
                    , HA.type_ "button"
                    ]
                    [ H.span [ HA.class "icon" ]
                        [ H.i [ HA.class "fas fa-plus" ] []
                        ]
                    , H.span [] [ H.text "Add website" ]
                    ]
                ]
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
            Just { name, websites } ->
                H.div [ HA.class "content" ]
                    [ H.h2 [ HA.class "title is-2" ] [ H.text "Output" ]
                    , H.p []
                        [ H.strong [] [ H.text "Name:" ]
                        , H.text " "
                        , H.text (Text.toString name)
                        ]
                    , websites
                        |> List.map
                            (\website ->
                                H.a
                                    [ HA.href (Text.toString website.address)
                                    , HA.target "_blank"
                                    ]
                                    [ H.text (Text.toString website.name) ]
                            )
                        |> List.intersperse (H.text ", ")
                        |> (++) [ H.strong [] [ H.text "Websites:" ], H.text " " ]
                        |> H.p []
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
