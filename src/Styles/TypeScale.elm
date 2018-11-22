module Styles.TypeScale exposing
    ( f1
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    )

import Css exposing (..)
import Html.Styled exposing (..)


f1 : Style
f1 =
    fontSize (Css.rem 3)


f2 : Style
f2 =
    fontSize (Css.rem 2.25)


f3 : Style
f3 =
    fontSize (Css.rem 1.5)


f4 : Style
f4 =
    fontSize (Css.rem 1.25)


f5 : Style
f5 =
    fontSize (Css.rem 1)


f6 : Style
f6 =
    fontSize (Css.rem 0.875)


f7 : Style
f7 =
    fontSize (Css.rem 0.75)
