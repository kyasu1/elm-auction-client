module Views.EditItem.PaymentMethod exposing (view)

import Data.Masters as Masters exposing (Masters)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.EditItem.Form exposing (ItemForm)
import Views.Form exposing (inputRadio, inputRest)
import Views.Style as Style


view : { r | form : ItemForm, masters : Masters } -> Html Form.Msg
view { form, masters } =
    let
        paymentMethod =
            Form.getFieldAsString "paymentMethod" form
    in
    div []
        [ label [ class Style.formLabel ] [ text "支払方法" ]
        , div [ class "flex flex-column flex-row-l justify-between" ]
            (List.map (viewPaymentMethod paymentMethod) (Masters.toTuple "payment_methods" masters))
        , p [ class "f6 dark-red" ]
            [ text "30万円以上の場合は代引きが利用出来ません。3万円以下の場合はオリコを選択しないで下さい。" ]
        ]


viewPaymentMethod : Form.FieldState e String -> ( String, String ) -> Html Form.Msg
viewPaymentMethod field ( id_, text_ ) =
    inputRadio text_ (Input.radioInput id_ field [ name "paymentMethod" ])
