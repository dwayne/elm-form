module SignUp.SimpleView exposing (main)

import Browser as B
import Field.Advanced as Field
import Form
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Browser.Dom as BD
import Lib.Bulma.Input
import SignUp.Email as Email
import SignUp.Error as Error
import SignUp.Form as SignUp
import SignUp.Password as Password
import SignUp.PasswordConfirmation as PasswordConfirmation
import SignUp.Username as Username


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
    { signUp : SignUp.Form
    , maybeOutput : Maybe SignUp.Output
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { signUp = SignUp.form
      , maybeOutput = Nothing
      }
    , focusUsername
    )



-- UPDATE


type Msg
    = Focus
    | InputUsername String
    | InputEmail String
    | InputPassword String
    | InputPasswordConfirmation String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus ->
            ( model, Cmd.none )

        InputUsername s ->
            ( { model | signUp = Form.modify .username (Field.setFromString s) model.signUp }
            , Cmd.none
            )

        InputEmail s ->
            ( { model | signUp = Form.modify .email (Field.setFromString s) model.signUp }
            , Cmd.none
            )

        InputPassword s ->
            ( { model | signUp = Form.modify .password (Field.setFromString s) model.signUp }
            , Cmd.none
            )

        InputPasswordConfirmation s ->
            ( { model | signUp = Form.modify .passwordConfirmation (Field.setFromString s) model.signUp }
            , Cmd.none
            )

        Submit ->
            ( { model | signUp = SignUp.form, maybeOutput = Form.validateAsMaybe model.signUp }
            , focusUsername
            )


focusUsername : Cmd Msg
focusUsername =
    BD.focus "username" Focus



-- VIEW


view : Model -> H.Html Msg
view { signUp, maybeOutput } =
    viewCenter
        [ H.h1 [ HA.class "title is-1" ] [ H.text "Sign Up" ]
        , H.form
            [ HA.class "block"
            , HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ Lib.Bulma.Input.view
                { id = "username"
                , label = "Username"
                , tipe = Lib.Bulma.Input.Text
                , field = Form.get .username signUp
                , errorToString = Error.usernameErrorToString
                , isRequired = True
                , isDisabled = False
                , onInput = InputUsername
                , attrs = [ HA.autofocus True ]
                }
            , Lib.Bulma.Input.view
                { id = "email"
                , label = "Email"
                , tipe = Lib.Bulma.Input.Email
                , field = Form.get .email signUp
                , errorToString = Error.emailErrorToString
                , isRequired = True
                , isDisabled = False
                , onInput = InputEmail
                , attrs = []
                }
            , Lib.Bulma.Input.view
                { id = "password"
                , label = "Password"
                , tipe = Lib.Bulma.Input.Password
                , field = Form.get .password signUp
                , errorToString = Error.passwordErrorToString
                , isRequired = True
                , isDisabled = False
                , onInput = InputPassword
                , attrs = []
                }
            , Lib.Bulma.Input.view
                { id = "passwordConfirmation"
                , label = "Password Confirmation"
                , tipe = Lib.Bulma.Input.Password
                , field = Form.get .passwordConfirmation signUp
                , errorToString = Error.passwordConfirmationErrorToString
                , isRequired = True
                , isDisabled = False
                , onInput = InputPasswordConfirmation
                , attrs = []
                }
            , H.div [ HA.class "field" ]
                [ H.div [ HA.class "control" ]
                    [ H.button
                        [ HA.class "button is-link"
                        , HA.disabled <| Form.isInvalid signUp
                        ]
                        [ H.text "Sign Up" ]
                    ]
                ]
            ]
        , case maybeOutput of
            Just { username, email, password } ->
                H.div [ HA.class "content" ]
                    [ H.h2 [ HA.class "title is-2" ] [ H.text "Output" ]
                    , H.p []
                        [ H.strong [] [ H.text "Username:" ]
                        , H.text " "
                        , H.text (Username.toString username)
                        ]
                    , H.p []
                        [ H.strong [] [ H.text "Email:" ]
                        , H.text " "
                        , H.text (Email.toString email)
                        ]
                    , H.p []
                        [ H.strong [] [ H.text "Password:" ]
                        , H.text " "
                        , H.text (Password.toString password)
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
