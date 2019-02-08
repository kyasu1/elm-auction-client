module Page.Register exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Http
import Request.GraphQL.Error exposing (Error(..), convertHttpError, httpErrorToString)
import Request.User as User
import Route exposing (Route)
import Task exposing (Task)
import Views.Form exposing (inputPassword, inputText)
import Views.Style as Style


type alias Model =
    { form : Form FormError FormData
    , loading : Bool
    , errors : List (Dict String String)
    }


type alias FormData =
    { name : String
    , email : String
    , password : String
    , passwordConfirm : String
    }


type FormError
    = NoMatchPassword
    | EmptyName
    | InvalidEmail
    | InvalidPasswordLength
    | EmptyPassword


customErrorToString : FormError -> String
customErrorToString error =
    case error of
        NoMatchPassword ->
            "パスワードが一致しません"

        EmptyName ->
            "お名前が入力されていません"

        InvalidEmail ->
            "メールアドレスを正しく入力してください"

        InvalidPasswordLength ->
            "パスワードが短すぎます"

        EmptyPassword ->
            "パスワードが入力されていません"


initialModel : Model
initialModel =
    { form = Form.initial initialFormData validation
    , loading = False
    , errors = []
    }


initialFormData : List ( String, Field )
initialFormData =
    [ ( "name", Field.string "" )
    , ( "email", Field.string "" )
    , ( "password", Field.string "" )
    , ( "passwordConfirm", Field.string "" )
    ]


validation : Validation FormError FormData
validation =
    map4 FormData
        (field "name" (string |> withCustomError EmptyName))
        (field "email"
            (email
                |> andThen (minLength 4)
                |> andThen (maxLength 40)
                |> withCustomError InvalidEmail
            )
        )
        (field "password"
            (string
                |> withCustomError EmptyPassword
                |> andThen (minLength 6)
                |> andThen (maxLength 100)
                |> withCustomError InvalidPasswordLength
            )
        )
        (field "password" string |> andThen validateConfirmation)


validateConfirmation : String -> Validation FormError String
validateConfirmation password =
    field "passwordConfirm"
        (string
            |> andThen
                (\confirmation ->
                    if password == confirmation then
                        Validate.succeed confirmation

                    else
                        fail (customError NoMatchPassword)
                )
        )



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    section [ class "flex items-center justify-center flex-column flex-auto" ] <|
        [ viewForm model |> Html.map FormMsg
        , div [ classList [ ( "loading", model.loading ) ] ] []
        ]


viewForm : Model -> Html Form.Msg
viewForm model =
    article [ class "mw5-ns ba-ns pv3-ns ph4-ns br3-ns b--light-gray w-90 mw6-ns" ]
        [ h3 [ class "tc" ] [ text "ユーザー登録" ]

        -- , ul [] (List.map (\e -> li [] [ text (Debug.toString e) ]) model.errors)
        , div []
            (inputText { label = "メールアドレス", optional = False }
                customErrorToString
                (Form.getFieldAsString "email" model.form)
            )
        , div []
            (inputText { label = "ユーザー名", optional = False }
                customErrorToString
                (Form.getFieldAsString "name" model.form)
            )
        , div []
            (inputPassword { label = "パスワード", optional = False }
                customErrorToString
                (Form.getFieldAsString "password" model.form)
            )
        , div []
            (inputPassword { label = "パスワード確認", optional = False }
                customErrorToString
                (Form.getFieldAsString "passwordConfirm" model.form)
            )
        , a
            [ classList
                [ ( Style.buttonDarkBlue, True )
                , ( "pointer dim", List.length (Form.getErrors model.form) == 0 )
                , ( "pointer-events-none o-50", Form.isSubmitted model.form && List.length (Form.getErrors model.form) > 0 )
                ]
            , onClick Form.Submit
            ]
            [ text "登\u{3000}録" ]
        ]



-- UPDATE --


type Msg
    = FormMsg Form.Msg
    | UserResponse (Result Error (User AuthToken))


type ExternalMsg
    = NoOp
    | SetUser (User AuthToken)


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just formData ) ->
                    ( ( { model | loading = True, errors = [] }
                      , User.register session formData
                            |> Task.attempt UserResponse
                      )
                    , NoOp
                    )

                _ ->
                    ( ( { model | form = Form.update validation formMsg model.form }, Cmd.none ), NoOp )

        UserResponse (Ok user) ->
            ( ( { model
                    | loading = False
                    , errors = []
                }
              , Cmd.batch
                    [ User.storeSession user
                    , Route.replaceUrl session.key <| Route.Feed user.id
                    ]
              )
            , SetUser user
            )

        UserResponse (Err err) ->
            let
                errors =
                    []

                --                    case err of
                --                        GraphQLError list ->
                --                             .locations list
                --
                --                        HttpError error ->
                --                            [ httpErrorToString error ]
            in
            ( ( { model
                    | loading = False
                    , errors = errors
                }
              , Cmd.none
              )
            , NoOp
            )
