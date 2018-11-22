module Views.Form exposing
    ( baseInput3
    , inputMoney
    , inputPassword
    , inputRadio
    , inputRest
    , inputRest3
    , inputSelect
    , inputText
    , radioButton
    , textarea
    , validateHelper
    )

import Form
import Form.Error exposing (ErrorValue)
import Form.Field
import Form.Input
import Html exposing (Html, div, label, p, span, text)
import Html.Attributes exposing (..)
import Util exposing (restLength)
import Views.Style as Style



-- tach =
--     { textInput = "input-reset ba b--black-20 pa2 mb1 db w-100"
--     , selectInput = "input-reset ba b--black-20 pa2 mb2 db w-100 f6"
--     , formLabel = "f6 b db mb1 mt2"
--     , button = "f6 link br3 ph3 pv2 mv2 dib dim tc w-100"
--     , buttonDarkBlue = "f6 link br3 ph3 pv2 mv2 dib white bg-dark-blue tc w-100"
--     }


error : Form.FieldState e String -> Bool
error field =
    field.liveError |> Maybe.map (\a -> True) |> Maybe.withDefault False


validateHelper : String -> Form.FieldState e String -> Html msg
validateHelper errorText field =
    span [ class "f6 db h1 mb1 b dark-red" ]
        -- [ if error field then
        --     text errorText
        --   else
        --     text ""
        [ text (field.liveError |> Maybe.map (\a -> Debug.toString a) |> Maybe.withDefault "")
        ]


baseInput :
    { r | label : String, error : String, optional : Bool }
    -> Html msg
    -> Form.FieldState e String
    -> List (Html msg)
baseInput config content field =
    let
        errorMessage =
            validateHelper config.error field
    in
    [ label [ for ("form-field-" ++ field.path), class Style.formLabel ]
        [ text config.label
        , span [ classList [ ( "normal black-60", True ), ( "dn", not config.optional ) ] ] [ text "（任意入力）" ]
        ]
    , content
    , errorMessage
    ]


baseInput2 :
    { r | label : String, optional : Bool }
    -> Html msg
    -> (e -> String)
    -> Form.FieldState e String
    -> List (Html msg)
baseInput2 config content errorToString field =
    [ label [ for ("form-field-" ++ field.path), class Style.formLabel ]
        [ text config.label
        , span
            [ classList
                [ ( "normal black-60", True )
                , ( "dn", not config.optional )
                ]
            ]
            [ text "（任意入力）" ]
        ]
    , content
    , span [ class "f6 db h1 mb1 b dark-red" ]
        [ text
            (field.liveError
                |> Maybe.map
                    (\e ->
                        case e of
                            Form.Error.CustomError customError ->
                                errorToString customError

                            _ ->
                                Debug.toString e
                    )
                |> Maybe.withDefault ""
            )
        ]
    ]


errorToString3 : Form.Error.ErrorValue String -> String
errorToString3 error3 =
    case error3 of
        Form.Error.CustomError e ->
            e

        _ ->
            "ERROR"


baseInput3 :
    { r | label : String, required : Bool, type_ : String, name : String }
    -> Form.FieldState String String
    -> Html Form.Msg
baseInput3 config field =
    let
        maybeError =
            field.liveError |> Maybe.map errorToString3
    in
    div [ class "flex-1" ]
        [ label [ class "font-black mb-8 block" ]
            [ div [ class "mb-2 text-lg" ]
                [ text config.label
                , if config.required == True then
                    text "(必須)"

                  else
                    text ""
                ]
            , Form.Input.baseInput config.type_ Form.Field.String Form.Text field [ name config.name, class "border py-2 px-3 text-grey-darkest w-full" ]
            ]
        , case maybeError of
            Just errorString ->
                p [ class "-mt-4 text-red pb-2" ] [ text errorString ]

            Nothing ->
                text ""
        ]



--


inputText :
    { r | label : String, optional : Bool }
    -> (e -> String)
    -> Form.FieldState e String
    -> List (Html Form.Msg)
inputText config errorToString field =
    let
        content =
            p [ class "flex mv0" ]
                [ Form.Input.textInput field
                    [ id ("form-field-" ++ field.path)
                    , classList
                        [ ( Style.textInput, True )
                        , ( "", True )
                        , ( "border border-solid border-red", error field )
                        ]
                    ]
                ]
    in
    baseInput2 config content errorToString field



--


inputPassword :
    { r | label : String, optional : Bool }
    -> (e -> String)
    -> Form.FieldState e String
    -> List (Html Form.Msg)
inputPassword config errorToString field =
    let
        content =
            p [ class "flex mv0" ]
                [ Form.Input.passwordInput field
                    [ id ("form-field-" ++ field.path)
                    , classList
                        [ ( Style.textInput, True )
                        , ( "flex-auto br--left-2", True )
                        , ( "b--dark-red", error field )
                        ]
                    ]
                ]
    in
    baseInput2 config content errorToString field


inputSelect :
    { r | label : String, optional : Bool }
    -> List ( String, String )
    -> (e -> String)
    -> Form.FieldState e String
    -> List (Html Form.Msg)
inputSelect config options errorToString field =
    let
        content =
            p [ class "flex mv0" ]
                [ Form.Input.selectInput options
                    field
                    [ id ("form-field-" ++ field.path)
                    , classList
                        [ ( Style.textInput, True )
                        , ( "flex-auto br--left-2", True )
                        , ( "b--dark-red", error field )
                        ]
                    ]
                ]
    in
    baseInput2 config content errorToString field



-- Input.selectInput
--     (( "", "選択して下さい" ) :: List.map (\user -> ( user.email, user.name )) model.userList)
--     (Form.getFieldAsString "email" model.form)
--     [ classList
--         [ ( Style.textInput, True )
--         , ( "b--dark-red", False )
--         ]
--     , required True
--     ]
--
-- residualCountInput :
--   { max : Float, label : String, error : String, optiona : Bool}
--   -> Form.FieldState e String
--   -> List (Html Form.Msg)
-- residualCountInput config field =
--   let
--       parmas = {
--         residual = Maybe.withDefault "" field.value |> restLength config.max
--       max = config.max
--       id
--   in
--       inputRest config field


inputRest :
    { label : String, error : String, optional : Bool }
    -> (String -> Float)
    -> Form.FieldState e String
    -> List (Html Form.Msg)
inputRest config counter field =
    let
        rest =
            Maybe.withDefault "" field.value
                |> counter

        content =
            p [ class "flex mv0" ]
                [ Form.Input.textInput field
                    [ id ("form-field-" ++ field.path)
                    , classList
                        [ ( Style.textInput, True )
                        , ( "flex-auto br--left-2", True )
                        , ( "b--dark-red", error field )
                        ]
                    ]
                , span
                    [ classList
                        [ ( "ba pa2 mb2 db white tc br--right-2", True )
                        , ( "bg-blue b--blue", rest >= 0 )
                        , ( "bg-dark-red b--dark-red", rest < 0 )
                        ]
                    , style "width" "8rem"
                    ]
                    [ text "残り"
                    , span [ class "dib tr w2 mr1" ] [ Debug.toString rest |> text ]
                    , text "文字"
                    ]
                ]
    in
    baseInput config content field


inputRest3 :
    { label : String, required : Bool, type_ : String, name : String }
    -> (String -> Float)
    -> Form.FieldState String String
    -> List (Html Form.Msg)
inputRest3 config counter field =
    let
        rest =
            Maybe.withDefault "" field.value
                |> counter
    in
    [ p [ class "flex" ]
        [ baseInput3 config field
        , span
            [ classList
                [ ( "ba pa2 mb2 db white tc br--right-2", True )
                , ( "bg-blue", rest >= 0 )
                , ( "bg-red-dark ", rest < 0 )
                ]
            , style "width" "8rem"
            ]
            [ text "残り"
            , span [ class "dib tr w2 mr1" ] [ String.fromFloat rest |> text ]
            , text "文字"
            ]
        ]
    ]



--


inputMoney :
    { label : String, error : String, optional : Bool, isDisabled : Bool }
    -> Form.FieldState e String
    -> List (Html Form.Msg)
inputMoney config field =
    let
        content =
            p [ class "flex mv0" ]
                [ Form.Input.textInput field
                    [ id ("form-field-" ++ field.path)
                    , classList
                        [ ( Style.textInput, True )
                        , ( "flex-auto br--left-2 tr", True )
                        , ( "b--dark-red", error field )
                        , ( "bg-black-30", config.isDisabled )
                        ]
                    , maxlength 12
                    , disabled config.isDisabled
                    , value <| Maybe.withDefault "" field.value
                    ]
                , span
                    [ classList
                        [ ( "ba bl-0 pa2 mb2 db b--black-20 tc br--right-2 w2", True )
                        ]
                    ]
                    [ text "円"
                    ]
                ]
    in
    baseInput config content field


inputRadio : String -> Html a -> Html a
inputRadio text_ content =
    div [ class "mr2" ]
        [ label [ class "f6 db mb1 mt2 " ]
            [ content
            , span [ class "ml2" ] [ text text_ ]
            ]
        ]



-- TEXTAREA --


textarea :
    { label : String, error : String, optional : Bool }
    -> Form.FieldState e String
    -> List (Html Form.Msg)
textarea config field =
    let
        content =
            Form.Input.textArea field
                [ classList
                    [ ( Style.textInput, True )
                    , ( "pa2 vh-50 br--right-2 br--left-2", True )
                    ]
                ]
    in
    baseInput config content field


radioButton : Form.FieldState e String -> ( String, String ) -> Html Form.Msg
radioButton field ( v, l ) =
    div [ class "mr2" ]
        [ label [ class "f6 db mb1 mt2 " ]
            [ Form.Input.radioInput v field []
            , span [ class "ml2" ] [ text l ]
            ]
        ]
