module Styles.Skins exposing
    ( black
    , error
    , gray
    , lightGray
    , nearWhite
    , primary
    , secondary
    , success
    , teritary
    , warning
    , white
    )

import Css exposing (..)


primary : Css.Color
primary =
    --    hex "76C4E2"
    hex "357edd"


secondary : Css.Color
secondary =
    hex "408BC9"


teritary : Css.Color
teritary =
    hex "00449E"


black : Css.Color
black =
    hex "000000"


gray : Css.Color
gray =
    hex "777777"


lightGray : Css.Color
lightGray =
    hex "EEEEEE"


success : Css.Color
success =
    hex "137752"


warning : Css.Color
warning =
    hex "FFD700"


error : Css.Color
error =
    hex "E7040F"


nearWhite : Css.Color
nearWhite =
    hex "F4F4F4"


white : Css.Color
white =
    hex "FFFFFF"
