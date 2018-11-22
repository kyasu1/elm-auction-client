module Views.Style exposing (button, buttonBase, buttonDarkBlue, buttonDisabled, buttonEnabled, buttonGray, buttonH, buttonSmall, formLabel, selectInput, textInput)

-- tach =
--     { textInput = "input-reset ba b--black-20 pa2 mb1 db w-100"
--     , selectInput = "input-reset ba b--black-20 pa2 mb2 db w-100 f6"
--     , formLabel = "f6 b db mb1 mt2"
--     , button = "f6 link br3 ph3 pv2 mv2 dib dim tc w-100"
--     , buttonDarkBlue = "f6 link br3 ph3 pv2 mv2 dib white bg-dark-blue tc w-100"
--     }


textInput =
    "input-reset ba b--black-20 pa2 mb2 db w-100"


selectInput =
    "input-reset ba b--black-20 pa2 mb2 db w-100 f6"


formLabel =
    "f6 b db mb1 mt2"



{--removes pre defined browser css from the button
--}


buttonBase =
    "b-transparent outline-0 input-reset pointer bn"


button =
    "f6 link br3 ph3 pv2 mv2 dib dim tc w-100"


buttonH : Bool -> String
buttonH hide =
    case hide of
        True ->
            "dn"

        False ->
            "f6 link br3 ph3 pv2 mv2 dib dim tc w-100"


buttonDarkBlue =
    button ++ " " ++ "white bg-dark-blue"



-- buttonDarkBlue =
--     "f6 link br3 ph3 pv2 mv2 dib white bg-dark-blue tc w-100"


buttonSmall =
    "f6 link br2 ph2 pv1 dib dim tc w-100 pointer"


buttonEnabled =
    "f4 link dim br2 w3 ph2 pv1 hover-dark-gray white tc pointer bg-light-silver"


buttonDisabled =
    "f4 br2 w3 ph2 pv1 white tc o-50 bg-light-silver"


buttonGray =
    "f4 link dim br2 w3 ph2 pv1 hover-dark-gray white tc pointer bg-light-silver"
