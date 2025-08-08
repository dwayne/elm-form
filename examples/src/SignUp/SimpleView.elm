module SignUp.SimpleView exposing (main)

import Browser as B
import Field.Advanced as Field
import Form
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Bulma.Field
import SignUp.Email as Email
import SignUp.Form as SignUp
import SignUp.Password as Password
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
            { model | maybeOutput = Form.validateAsMaybe model.signUp }



-- VIEW


view : Model -> H.Html Msg
view { signUp, maybeOutput } =
    let
        fields =
            Form.toFields signUp
    in
    H.div []
        [ H.h2 [] [ H.text "Sign Up" ]
        , H.form
            [ HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ Lib.Bulma.Field.view
                { id = "username"
                , label = "Username"
                , tipe = Lib.Bulma.Field.Text
                , field = fields.username
                , errorToString = usernameErrorToString
                , isRequired = True
                , isDisabled = False
                , inputAttrs = [ HA.autofocus True ]
                , onInput = InputUsername
                }

            --, Lib.Bulma.Field.view
            --    { id = "email"
            --    , label = "Email"
            --    , tipe = "text"
            --    , field = fields.email
            --    , errorToString = Email.errorToString
            --    , isRequired = True
            --    , isDisabled = False
            --    , inputAttrs = []
            --    , onInput = InputUsername
            --    }
            --, Lib.Bulma.Field.view
            --    { id = "password"
            --    , label = "Password"
            --    , tipe = "password"
            --    , field = fields.password
            --    , errorToString = Password.errorToString
            --    , isRequired = True
            --    , isDisabled = False
            --    , inputAttrs = []
            --    , onInput = InputPassword
            --    }
            --, Lib.Bulma.Field.view
            --    { id = "passwordConfirmation"
            --    , label = "Password Confirmation"
            --    , tipe = "password"
            --    , field = fields.passwordConfirmation
            --    , errorToString = PasswordConfirmation.errorToString
            --    , isRequired = True
            --    , isDisabled = False
            --    , inputAttrs = []
            --    , onInput = InputPasswordConfirmation
            --    }
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
                H.div []
                    [ H.h2 [] [ H.text "Output" ]
                    , H.p [] [ H.text <| "Username: " ++ Username.toString username ]
                    , H.p [] [ H.text <| "Email: " ++ Email.toString email ]
                    , H.p [] [ H.text <| "Password: " ++ Password.toString password ]
                    ]

            Nothing ->
                H.text ""
        ]


usernameErrorToString : Username.Error -> String
usernameErrorToString =
    Field.errorToString
        { onBlank = "It is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \error ->
                case error of
                    Username.TooShort { actual, min } ->
                        "It must be at least " ++ String.fromInt min ++ " characters in length: " ++ String.fromInt actual ++ "."

                    Username.TooLong { actual, max } ->
                        "It must be at most " ++ String.fromInt max ++ " characters in length: " ++ String.fromInt actual ++ "."
        }
