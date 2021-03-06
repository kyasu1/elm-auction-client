module Views.EditItem.Condition exposing (view)

import Data.Condition as Condition
import Data.Masters as Masters exposing (Masters)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import UI.Button as UI
import Util exposing (restSJISLength)
import Views.EditItem.Form exposing (ItemForm, errorToString)
import Views.Form exposing (inputRest, radioOption)
import Views.Style as Style


view : { r | form : ItemForm, masters : Masters } -> Html Form.Msg
view { form, masters } =
    let
        condition =
            Form.getFieldAsString "condition" form

        conditions =
            Masters.toTuple "conditions" masters
                |> List.filter (\( id, _ ) -> id /= "2")
                |> List.filter (\( id, _ ) -> id /= "3")
    in
    div [ class "flex flex-col" ]
        [ label [ class Style.formLabel ] [ text "商品の状態" ]
        , div [ class "flex flex-col md:flex-row md:flex-wrap" ]
            (List.map (radioOption condition "condition") conditions)
        ]



--
-- viewCodition : Form.FieldState e String -> ( String, String ) -> Html Form.Msg
-- viewCodition field ( id_, text_ ) =
--     radioOption text_ (Input.radioInput id_ field [ name "condition" ])
