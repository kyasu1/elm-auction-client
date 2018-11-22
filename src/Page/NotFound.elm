module Page.NotFound exposing (view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)


view : Session -> Html msg
view session =
    section [ class "flex items-center justify-start flex-column flex-auto" ]
        [ article [ class "bl-ns br-ns pv3-ns ph4-ns b--light-gray w-90 mw8-ns flex-auto" ]
            [ h1 [] [ text "Page Not Found" ]
            ]
        ]
