module Util exposing
    ( addTax
    , addTax008
    , addTax010
    , restLength
    , restSJISLength
    , viewIf
    )

import Char
import Html exposing (Html)


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content

    else
        Html.text ""


countSJISBytes : String -> Float
countSJISBytes str =
    let
        list =
            String.toList str

        f char acc =
            let
                code =
                    Char.toCode char
            in
            if code >= 0x00 && code < 0x81 then
                acc + 1

            else if code == 0xA0 then
                acc + 1

            else if code >= 0xA1 && code < 0xDF then
                acc + 1

            else if code >= 0xFD && code < 0xFF then
                acc + 1

            else
                acc + 2
    in
    List.foldl f 0 list


restLength : Float -> String -> Float
restLength max str =
    max - (String.length str |> toFloat)


restSJISLength : Float -> String -> Float
restSJISLength max str =
    let
        len =
            countSJISBytes str
    in
    max - (len / 2)


addTax : Float -> Int -> Int
addTax r v =
    v + (toFloat v * r |> round)


addTax008 : Int -> Int
addTax008 =
    addTax 0.08


addTax010 : Int -> Int
addTax010 =
    addTax 0.1
