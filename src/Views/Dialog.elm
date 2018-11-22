module Views.Dialog exposing (ButtonConfig, Dialog, closableDialog, loadingDialog, subscriptions, view, yesNoDialog)

import Browser.Events exposing (onKeyPress, onKeyUp)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import UI.Button exposing (..)


type alias Dialog msg =
    Maybe (Html msg)


view : Dialog msg -> Html msg
view dialog =
    case dialog of
        Just content ->
            div
                [ class "fixed pin z-50 overflow-auto bg-smoke-light flex"
                , Html.Attributes.attribute "role" "dialog"
                , Html.Attributes.attribute "aria-modal" "true"
                , style "transition" "all 3s ease"
                ]
                [ div [ class "relative p-8 bg-white w-full max-w-md m-auto flex-col flex", style "transform" "translateY(0)" ]
                    [ content
                    ]
                ]

        Nothing ->
            text ""



--type alias Config =
--  { title : Maybe String
--  , showCloseButton : Bool
--  ,
--dialog title content actions =
--    div
--        [ class "fixed top-0 left-0 w-100 h-100 z-999 o-100 flex bg-black-70"
--        , style [ ( "transition", "all 3s ease" ) ]
--        ]
--        [ div
--            [ class "w-50 h-50 self-center bg-moon-gray br3 ph2 pv2 center mv2 shadow-3 z-9999 flex flex-column justify-center"
--            , style [ ( "transform", "translateY(0)" ) ]
--            ]
--            [ title
--            , content
--            , actions
--            ]
--        ]


loadingDialog : String -> Html msg
loadingDialog message =
    div []
        [ text message
        ]


closableDialog : String -> msg -> Html msg
closableDialog header closeMessage =
    div []
        [ text header
        , button [ onClick closeMessage ] [ text "閉じる" ]
        ]


type alias ButtonConfig msg =
    { message : msg
    , label : String
    }


yesNoDialog : String -> Html msg -> ButtonConfig msg -> ButtonConfig msg -> Html msg
yesNoDialog header content buttonYes buttonNo =
    div
        [ class "flex flex-col"
        ]
        [ h2 [] [ text header ]
        , hr [ class "h-px w-full bg-black" ] []
        , content
        , div []
            [ darkButton { label = buttonNo.label, icon = "fas fa-ban" } buttonNo.message
            , darkButton { label = buttonYes.label, icon = "fas fa-check" } buttonYes.message
            ]
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : { close : msg, none : msg } -> Sub msg
subscriptions { close, none } =
    onKeyUp keyDecoder
        |> Sub.map
            (\string ->
                case string of
                    "Escape" ->
                        close

                    _ ->
                        none
            )
