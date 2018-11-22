module Style exposing
    ( BtnConfig
    , btnBase
    , defaultBtnConfig
    , mediaL
    , mediaM
    , mediaN
    )

-- import Css.Transitions

import Css exposing (..)
import Css.Colors as Colors
import Css.Media as Media exposing (only, screen, withMedia)
import Html.Styled exposing (..)
import Styles.Skins as Skins
import Styles.Spacing as Spacing
import Styles.TypeScale exposing (..)


{-| non small
-}
mediaN : List Style -> Style
mediaN =
    withMedia [ only screen [ Media.minWidth (Css.em 30) ] ]


{-| medium only
-}
mediaM : List Style -> Style
mediaM =
    withMedia [ only screen [ Media.minWidth (Css.em 30), Media.maxWidth (Css.em 60) ] ]


{-| large only
-}
mediaL : List Style -> Style
mediaL =
    withMedia [ only screen [ Media.minWidth (Css.em 60) ] ]


type alias BtnConfig =
    { foreground : Color
    , background : Color
    , disabled : Bool
    , spacing : Rem
    }


defaultBtnConfig : BtnConfig
defaultBtnConfig =
    { foreground = Skins.white
    , background = Skins.primary
    , disabled = False
    , spacing = Spacing.s2
    }


btnBase : BtnConfig -> Style
btnBase { foreground, background, disabled, spacing } =
    let
        base =
            [ f6
            , displayFlex
            , alignItems center
            , justifyContent center
            , textDecoration none
            , width (pct 100)
            , hover [ opacity (Css.num 0.5) ]
            , focus [ outline zero ]
            , color foreground
            , backgroundColor background
            , padding2 spacing zero
            , margin2 Spacing.s1 zero
            , border (px 0)
            , borderRadius Spacing.s2
            , outline none
            ]
    in
    if disabled == True then
        Css.batch <| base ++ [ opacity (Css.num 0.5), cursor auto, Css.disabled [] ]

    else
        Css.batch <| base ++ [ opacity (Css.num 1), cursor pointer ]
