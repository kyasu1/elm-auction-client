module Views.Style exposing
    ( button
    , buttonBase
    , buttonDarkBlue
    ,  buttonDisabled
       -- , buttonEnabled

    , buttonGray
    , buttonH
    , buttonSmall
    , buttonSmallBlue
    , buttonSmallGrey
    , buttonSmallRed
    , formLabel
    , selectInput
    , textInput
    )


textInput =
    "input-reset ba b--black-20 pa2 mb2 db w-100"


selectInput =
    "input-reset ba b--black-20 pa2 mb2 db w-100 f6"


formLabel =
    "font-bold mt-2"



{--removes pre defined browser css from the button
--}


buttonBase =
    "w-full md:w-32 cursor-pointer inline-block no-underline flex items-center justify-center"


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
    buttonBase ++ " h-8 text-sm "


buttonSmallBlue =
    buttonSmall ++ " bg-blue hover:bg-blue-dark text-white"


buttonSmallRed =
    buttonSmall ++ " bg-red hover:bg-red-dark text-white"


buttonSmallGrey =
    buttonSmall ++ " bg-grey hover:bg-grey-dark text-white"


buttonDisabled =
    "f4 br2 w3 ph2 pv1 white tc o-50 bg-light-silver"


buttonGray =
    "f4 link dim br2 w3 ph2 pv1 hover-dark-gray white tc pointer bg-light-silver"
