module SignUp.SimpleView exposing (main)

import Browser as B
import Field.Advanced as Field
import Form
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Bulma.Field
import SignUp.Email as Email
import SignUp.Error as Error
import SignUp.Form as SignUp
import SignUp.Password as Password
import SignUp.PasswordConfirmation as PasswordConfirmation
import SignUp.Username as Username


main : Program () Model Msg
main =
    B.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { signUp : SignUp.Form
    , maybeOutput : Maybe SignUp.Output
    }


init : Model
init =
    { signUp = SignUp.form
    , maybeOutput = Nothing
    }



-- UPDATE


type Msg
    = InputUsername String
    | InputEmail String
    | InputPassword String
    | InputPasswordConfirmation String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputUsername s ->
            { model | signUp = Form.update .setUsername s model.signUp }

        InputEmail s ->
            { model | signUp = Form.update .setEmail s model.signUp }

        InputPassword s ->
            { model | signUp = Form.update .setPassword s model.signUp }

        InputPasswordConfirmation s ->
            { model | signUp = Form.update .setPasswordConfirmation s model.signUp }

        Submit ->
            { model | signUp = SignUp.form, maybeOutput = Form.validateAsMaybe model.signUp }



-- VIEW


view : Model -> H.Html Msg
view { signUp, maybeOutput } =
    let
        fields =
            Form.toFields signUp
    in
    viewCenter
        [ H.h1 [ HA.class "title is-1" ] [ H.text "Sign Up" ]
        , H.form
            [ HA.class "block"
            , HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ Lib.Bulma.Field.view
                { id = "username"
                , label = "Username"
                , tipe = Lib.Bulma.Field.Text
                , field = fields.username
                , errorToString = Error.usernameErrorToString
                , isRequired = True
                , isDisabled = False
                , inputAttrs = [ HA.autofocus True ]
                , onInput = InputUsername
                }
            , Lib.Bulma.Field.view
                { id = "email"
                , label = "Email"
                , tipe = Lib.Bulma.Field.Email
                , field = fields.email
                , errorToString = Error.emailErrorToString
                , isRequired = True
                , isDisabled = False
                , inputAttrs = []
                , onInput = InputEmail
                }
            , Lib.Bulma.Field.view
                { id = "password"
                , label = "Password"
                , tipe = Lib.Bulma.Field.Password
                , field = fields.password
                , errorToString = Error.passwordErrorToString
                , isRequired = True
                , isDisabled = False
                , inputAttrs = []
                , onInput = InputPassword
                }
            , Lib.Bulma.Field.view
                { id = "passwordConfirmation"
                , label = "Password Confirmation"
                , tipe = Lib.Bulma.Field.Password
                , field = fields.passwordConfirmation
                , errorToString = Error.passwordConfirmationErrorToString
                , isRequired = True
                , isDisabled = False
                , inputAttrs = []
                , onInput = InputPasswordConfirmation
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
