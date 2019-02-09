module Views.EditItem.PaymentMethod exposing (view)

import Data.Masters as Masters exposing (Masters)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.EditItem.Form exposing (ItemForm)
import Views.Form exposing (inputRest, radioOption)
import Views.Style as Style


view : { r | form : ItemForm, masters : Masters } -> Html Form.Msg
view { form, masters } =
    let
        paymentMethod =
            Form.getFieldAsString "paymentMethod" form
    in
    div [ class "flex flex-col" ]
        [ label [ class Style.formLabel ] [ text "支払方法" ]
        , div [ class "flex flex-col md:flex-row md:flex-wrap" ]
            (List.map (radioOption paymentMethod "paymentMethod") (Masters.toTuple "payment_methods" masters))
        , p [ class "font-bold text-red py-1" ]
            [ text "30万円以上の場合は代引きが利用出来ません。3万円以下の場合はオリコを選択しないで下さい。" ]
        ]
