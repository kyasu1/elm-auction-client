module UI.Button exposing
    ( baseInput
    , darkButton
    , darkButtonDisabled
    , darkLink
    , inputMoney
    , inputRest
    , textarea
    , textarea2
    )

import Form
import Form.Error
import Form.Field
import Form.Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route


darkLink : { label : String, icon : String } -> Route.Route -> Html msg
darkLink { label, icon } route =
    a
        [ class "block no-underline text-white bg-grey-darkest ml-2"
        , Route.href route
        ]
        [ i [ class <| String.join " " [ icon, "p-1 border-r" ] ] []
        , span [ class "p-1" ] [ text label ]
        ]


type ButtonType
    = Button
    | Submit
    | Reset


buttonTypeToString : ButtonType -> String
buttonTypeToString buttonType =
    case buttonType of
        Button ->
            "button"

        Submit ->
            "submit"

        Reset ->
            "reset"


baseButton : { buttonType : ButtonType, buttonClass : String, label : String, icon : String } -> msg -> Html msg
baseButton { buttonType, buttonClass, label, icon } msg =
    button
        [ type_ <| buttonTypeToString buttonType
        , class <| String.join " " [ buttonClass, "ml-2" ]
        , onClick msg
        ]
        [ i [ class <| String.join " " [ icon, "p-1 border-r" ] ] []
        , span [ class "p-1" ] [ text label ]
        ]


darkButton : { label : String, icon : String } -> msg -> Html msg
darkButton { label, icon } msg =
    button
        [ type_ "button"
        , class "text-white bg-grey-darkest ml-2"
        , onClick msg
        ]
        [ i [ class <| String.join " " [ icon, "p-1 border-r" ] ] []
        , span [ class "p-1" ] [ text label ]
        ]


classDisabled =
    "opacity-50 cursor-not-allowed"


darkButtonDisabled : { label : String, icon : String } -> msg -> Html msg
darkButtonDisabled { label, icon } msg =
    baseButton
        { buttonType = Button
        , buttonClass = String.join " " [ "text-white bg-grey-darkest", classDisabled ]
        , label = label
        , icon = icon
        }
        msg


errorToString : Form.Error.ErrorValue e -> (e -> String) -> String
errorToString error customErrorToString =
    case error of
        Form.Error.CustomError e ->
            customErrorToString e

        _ ->
            "ERROR"


labelWithRequired : String -> Bool -> Html msg -> Html msg
labelWithRequired labelString required child =
    label [ class "font-black mb-8 block" ]
        [ div [ class "my-2" ]
            [ text labelString
            , if required then
                text "(必須)"

              else
                text ""
            ]
        , child
        ]


baseInput :
    { r | label : String, required : Bool, type_ : String, name : String }
    -> (e -> String)
    -> Form.FieldState e String
    -> Html Form.Msg
baseInput config customErrorToString field =
    let
        maybeError =
            field.liveError |> Maybe.map errorToString

        baseClass =
            "border py-2 px-3 text-grey-darkest w-full"

        ( inputClass, error ) =
            case maybeError of
                Just errorString ->
                    ( String.join " " [ baseClass, "border-red" ]
                    , p
                        [ class "-mt-4 text-red pb-2" ]
                        [ text <| errorString customErrorToString ]
                    )

                Nothing ->
                    ( baseClass, text "" )
    in
    div [ class "flex-1" ]
        [ labelWithRequired config.label config.required <|
            Form.Input.baseInput config.type_
                Form.Field.String
                Form.Text
                field
                [ name config.name
                , class inputClass
                ]
        , error
        ]


inputRest :
    { label : String, required : Bool, name : String }
    -> (String -> Float)
    -> (e -> String)
    -> Form.FieldState e String
    -> Html Form.Msg
inputRest config counter customErrorToString field =
    let
        rest =
            Maybe.withDefault "" field.value
                |> counter
    in
    div [ class "flex" ]
        [ baseInput { label = config.label, required = config.required, type_ = "text", name = config.name } customErrorToString field
        , span
            [ classList
                [ ( "text-white p-2 my-8 h-10 border-t-2 border-b-2 border-white", True )
                , ( "bg-blue", rest >= 0 )
                , ( "bg-red-dark ", rest < 0 )
                ]
            , style "width" "8rem"
            ]
            [ text "残り"
            , span [ class "inline-block text-write w-4 mr-4" ] [ String.fromFloat rest |> text ]
            , text "文字"
            ]
        ]


inputMoney :
    { label : String, required : Bool, name : String }
    -> (e -> String)
    -> Form.FieldState e String
    -> Html Form.Msg
inputMoney config customErrorToString field =
    div [ class "flex " ]
        [ baseInput { label = config.label, required = config.required, type_ = "text", name = config.name }
            customErrorToString
            field
        , span
            [ class "p-2 my-8" ]
            [ text "円" ]
        ]


textarea :
    { label : String, required : Bool }
    -> Form.FieldState e String
    -> Html Form.Msg
textarea config field =
    labelWithRequired config.label config.required <|
        Form.Input.textArea field [ class "p-2 h-64 w-full leading-normal border text-grey-darkest" ]


labeled : Html msg -> Html msg -> Html msg
labeled labelChild bodyChild =
    label [ class "mb-8 block" ]
        [ labelChild
        , bodyChild
        ]


textarea2 : Html Form.Msg -> Form.FieldState e String -> Html Form.Msg
textarea2 label field =
    labeled label <|
        Form.Input.textArea field [ class "p-2 h-64 w-full leading-normal border text-grey-darkest" ]
