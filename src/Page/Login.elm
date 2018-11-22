module Page.Login exposing (ExternalMsg(..), Model, Msg, initialTask, update, view)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Input as Input
import Form.Validate as Validate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, required)
import Html.Events exposing (onClick)
import Http
import Request.GraphQL.Error as Error exposing (Error)
import Request.GraphQL.Helper as Helper
import Request.GraphQL.Query as Query
import Request.User as User
import Route exposing (Route)
import Task exposing (Task)
import Views.Form exposing (baseInput3, inputPassword, inputText)
import Views.Style as Style


type alias Model =
    { form : Form String FormData
    , userList : List (User ())
    , loading : Bool
    , error : Maybe String
    }


type alias FormData =
    { email : String
    , password : String
    , remember : Bool
    }


initialModel : List (User ()) -> Model
initialModel list =
    { form = Form.initial initialFormData validation
    , userList = list
    , loading = False
    , error = Nothing
    }


initialTask : Task Error Model
initialTask =
    User.list
        |> Task.map
            (\list ->
                initialModel list
            )


initialFormData : List ( String, Field )
initialFormData =
    [ ( "email", Field.string "" )
    , ( "password", Field.string "" )
    , ( "remember", Field.bool True )
    ]


validation : Validation String FormData
validation =
    map3 FormData
        (field "email"
            (email
                |> withCustomError "メールアドレスの形式に誤りがあります"
            )
        )
        (field "password"
            (string
                |> withCustomError "パスワードを入力してください"
            )
        )
        (field "remember" bool)



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    section [ class "flex items-center justify-center flex-column flex-auto" ] <|
        [ viewForm model |> Html.map FormMsg
        , div [ classList [ ( "loading", model.loading ) ] ] []
        ]


viewForm : Model -> Html Form.Msg
viewForm model =
    article [ classList [ ( "w-1/3", True ), ( "border-red", model.error /= Nothing ) ] ]
        [ h3 [ class "text-center py-4" ] [ text "ログイン" ]
        , case model.error of
            Nothing ->
                text ""

            Just error ->
                h4 [ class "mv2 tc red" ] [ text error ]
        , baseInput3 { label = "ユーザー名", required = True, type_ = "text", name = "email" }
            (Form.getFieldAsString "email" model.form)
        , baseInput3 { label = "パスワード", required = True, type_ = "password", name = "password" }
            (Form.getFieldAsString "password" model.form)
        , p [ class "py-4" ]
            [ label []
                [ Input.checkboxInput (Form.getFieldAsBool "remember" model.form) []
                , text "ログイン情報を保存する"
                ]
            ]
        , button
            [ classList
                [ ( "block bg-teal hover:bg-teal-dark text-white uppercase text-lg mx-auto p-2 rounded", True )
                , ( "pointer dim", List.length (Form.getErrors model.form) == 0 )
                , ( "pointer-events-none", Form.isSubmitted model.form && List.length (Form.getErrors model.form) > 0 )
                ]
            , onClick Form.Submit
            ]
            [ text "ログイン" ]
        ]



-- UPDATE --


type Msg
    = FormMsg Form.Msg
    | LoginResponse Bool (Result Error (User AuthToken))


type ExternalMsg
    = NoOp
    | SetUser (User AuthToken)


noOp : a -> ( a, ExternalMsg )
noOp a =
    ( a, NoOp )


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | loading = True }
                      --                      , User.login session form
                    , User.login { email = form.email, password = form.password }
                        |> Task.attempt (LoginResponse form.remember)
                    )
                        |> noOp

                _ ->
                    ( { model | form = Form.update validation formMsg model.form, error = Nothing }, Cmd.none )
                        |> noOp

        LoginResponse remember (Ok user) ->
            let
                storeCmd =
                    if remember then
                        User.storeSession user

                    else
                        Cmd.none
            in
            ( ( { model | loading = False, error = Nothing }
              , Cmd.batch [ storeCmd, Route.replaceUrl session.key <| Route.Feed user.id ]
              )
            , SetUser user
            )

        LoginResponse _ (Err e) ->
            ( { model
                | loading = False
                , error = Just "ログインエラー"
              }
            , Cmd.none
            )
                |> noOp
