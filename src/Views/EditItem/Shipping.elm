module Views.EditItem.Shipping exposing (form)

import Data.Shipping as Shipping exposing (Method(..), Payer(..), Size(..))
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.EditItem.Form exposing (ItemForm)
import Views.Form exposing (radioButton)
import Views.Style as Style


toTuple : String -> ( String, String )
toTuple s =
    ( s, s )


payers =
    [ "出品者", "落札者" ]
        |> List.map toTuple


methods =
    [ "普通郵便", "ネコポス", "コンパクト", "宅急便" ]
        |> List.map toTuple


sizes =
    [ "60サイズ", "80サイズ", "100サイズ", "120サイズ", "140サイズ", "160サイズ" ]
        |> List.map toTuple



-- FORM


form : ItemForm -> Html Form.Msg
form itemForm =
    let
        payer =
            Form.getFieldAsString "shipping.payer" itemForm
    in
    div []
        [ div []
            [ label [ class Style.formLabel ] [ text "送料負担" ]
            , div [ class "flex flex-column flex-row-ns" ]
                (List.map (radioButton payer) payers)
            ]
        , case payer.value of
            Just "出品者" ->
                div [] [ text "" ]

            Just "落札者" ->
                formBuyer itemForm

            _ ->
                div [] [ text "Invalid payer specified" ]
        ]


formBuyer : ItemForm -> Html Form.Msg
formBuyer itemForm =
    let
        method =
            Form.getFieldAsString "shipping.method" itemForm
    in
    div []
        [ label [ class Style.formLabel ] [ text "発送方法" ]
        , div [ class "flex flex-column flex-row-ns" ]
            (List.map (radioButton method) methods)
        , case method.value of
            Just "宅急便" ->
                formSize itemForm

            _ ->
                text ""
        ]


formSize : ItemForm -> Html Form.Msg
formSize itemForm =
    let
        size =
            Form.getFieldAsString "shipping.size" itemForm
    in
    div []
        [ label [ class Style.formLabel ] [ text "サイズ" ]
        , div [ class "flex flex-column flex-row-l" ]
            (List.map (radioButton size) sizes)
        , case size.error of
            Nothing ->
                text ""

            _ ->
                p [ class "f6 dark-red" ] [ text "サイズを指定してください" ]
        ]



-- TEST
--main : Html msg
--main =
--    div []
--        [ p [] [ decodeString decoder json1 |> toString |> text ]
--        , p [] [ decodeString decoder json2 |> toString |> text ]
--        , p [] [ decodeString decoder json3 |> toString |> text ]
--        , div [] [ view <| Buyer <| Takkyubin Yamato60 ]
--        ]
--
--
--json1 =
--    """
--    { "payer": "seller" }
--"""
--
--
--json2 =
--    """
--    { "payer": "buyer", "method": "1" }
--"""
--
--
--json3 =
--    """
--    { "payer": "buyer", "method": "4", "size": "60" }
--    """
--
--
--type alias Model =
--    { shippingPaidBy : Payer
--    }
