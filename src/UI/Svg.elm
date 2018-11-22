module UI.Svg exposing (noImage)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


noImage : Html msg
noImage =
    svg
        [ width "100%"
        , viewBox "0 0 100 100"
        ]
        [ rect
            [ x "0"
            , y "0"
            , width "100"
            , height "100"
            , fill "#DAE1E7"
            ]
            []
        , text_
            [ x "50%"
            , y "50%"
            , textAnchor "middle"
            , dominantBaseline "central"
            , class "fill-current text-blue"
            ]
            [ text "NO IMAGE" ]
        ]
