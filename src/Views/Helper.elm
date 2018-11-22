module Views.Helper exposing (toLongString, toPrice, toShortString, toYenPrice)

import Char
import Date exposing (Date, Month)
import Regex
import String.Extra
import Time exposing (Month(..))


toPrice : String -> Int -> String
toPrice suffix price =
    --    Regex.replace Regex.All (Regex.regex "(\\d)(?=(\\d{3})+$)") (\{ match } -> match ++ ",") (String.fromInt price) ++ suffix |> String.padLeft 12 ' '
    String.fromInt price
        |> String.reverse
        |> String.Extra.break 3
        |> List.map String.reverse
        |> List.reverse
        |> String.join ","
        |> (\formattedPrice -> formattedPrice ++ suffix)


toYenPrice : Int -> String
toYenPrice =
    toPrice "円"


toLongString : Date -> String
toLongString date =
    (Date.year date |> String.fromInt |> String.padLeft 4 ' ')
        ++ "/"
        ++ (Date.month date |> monthToNumberString |> String.padLeft 2 ' ')
        ++ "/"
        ++ (Date.day date |> String.fromInt |> String.padLeft 2 ' ')


toShortString : Date -> String
toShortString date =
    "'"
        ++ (Date.year date |> String.fromInt |> String.right 2)
        ++ "/"
        ++ (Date.month date |> monthToNumberString |> String.padLeft 2 ' ')
        ++ "/"
        ++ (Date.day date |> String.fromInt |> String.padLeft 2 ' ')


monthToNumberString : Date.Month -> String
monthToNumberString month =
    case month of
        Jan ->
            "1"

        Feb ->
            "2"

        Mar ->
            "3"

        Apr ->
            "4"

        May ->
            "5"

        Jun ->
            "6"

        Jul ->
            "7"

        Aug ->
            "8"

        Sep ->
            "9"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"
