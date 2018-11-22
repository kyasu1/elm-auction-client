module Views.EditItem.Selling exposing (view)

import Data.Masters as Masters exposing (Masters)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.EditItem.Form exposing (ItemForm)
import Views.Form exposing (inputMoney, radioButton)
import Views.Style as Style


sellingMethods : List ( String, String )
sellingMethods =
    [ "オークション形式", "即決形式" ] |> List.map (\s -> ( s, s ))


view : ItemForm -> Html Form.Msg
view form =
    let
        method =
            Form.getFieldAsString "selling.method" form
    in
    div [ class "flex flex-col md:flex-row" ]
        ([ div [ class "w-full md:w-1/2" ] <|
            [ label [ class Style.formLabel ] [ text "販売形式" ]
            , div [ class "flex" ]
                (List.map (radioButton method) sellingMethods)
            ]
         ]
            ++ (case method.value of
                    Just "オークション形式" ->
                        viewSellingAuction form

                    Just "即決形式" ->
                        viewSellingFixed form

                    _ ->
                        [ text "Something is wrong" ]
               )
        )


viewSellingAuction : ItemForm -> List (Html Form.Msg)
viewSellingAuction form =
    let
        priceOpen =
            Form.getFieldAsString "selling.priceOpen" form

        priceBuyout =
            Form.getFieldAsString "selling.priceBuyout" form
    in
    [ div [ class "w-full md:w-1/4 md:py-2" ] <|
        inputMoney
            { label = "開始価格"
            , error = "1以上の数字で入力してください"
            , optional = False
            , isDisabled = False
            }
            priceOpen
    , div [ class "w-full md:w-1/4 md:py-2" ] <|
        inputMoney
            { label = "即決価格"
            , error = "0または開始価格以上の数字で入力してください"
            , optional = False
            , isDisabled = False
            }
            priceBuyout
    ]


viewSellingFixed : ItemForm -> List (Html Form.Msg)
viewSellingFixed form =
    let
        priceBuyout =
            Form.getFieldAsString "selling.priceBuyout" form

        allowDiscount =
            Form.getFieldAsBool "selling.allowDiscount" form

        icon =
            case allowDiscount.value of
                Just True ->
                    i [ class "far fa-check-square" ] []

                _ ->
                    i [ class "far fa-square" ] []
    in
    [ div [ class "w-100 w-25-l ph1-l" ] <|
        inputMoney
            { label = "即決価格"
            , error = "0または開始価格以上の数字で入力してください"
            , optional = False
            , isDisabled = False
            }
            priceBuyout
    , div [ class "w-100 w-25-l ph1-l self-center" ]
        [ label [ for "allowDiscount", class "pt-2" ]
            [ icon
            , span [ class "" ] [ text "値下げ交渉を受ける" ]
            ]
        , Input.checkboxInput allowDiscount [ id "allowDiscount", class "hidden" ]
        ]
    ]
