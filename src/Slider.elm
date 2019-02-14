module Slider exposing
    ( view
    , Model, Msg, init, initialModel, subscriptions, update
    )

{-|

@docs Config, State
@docs config, initialState, moveImage, view

-}

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (class, id, src, style, tabindex)
import Html.Events exposing (keyCode, on, onClick)
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode exposing (Decoder, Value)
import Task
import UI.Svg exposing (noImage)


btnRight : String
btnRight =
    "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz48c3ZnIHdpZHRoPSIxN3B4IiBoZWlnaHQ9IjIycHgiIHZpZXdCb3g9IjAgMCAxNyAyMiIgdmVyc2lvbj0iMS4xIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj4gICAgICAgIDx0aXRsZT5hbmdsZWQtcmlnaHQ8L3RpdGxlPiAgICA8ZGVzYz5DcmVhdGVkIHdpdGggU2tldGNoLjwvZGVzYz4gICAgPGRlZnM+PC9kZWZzPiAgICA8ZyBpZD0iUGFnZS0xIiBzdHJva2U9Im5vbmUiIHN0cm9rZS13aWR0aD0iMSIgZmlsbD0ibm9uZSIgZmlsbC1ydWxlPSJldmVub2RkIj4gICAgICAgIDxnIGlkPSIyNCIgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoLTQuMDAwMDAwLCAtMS4wMDAwMDApIj4gICAgICAgICAgICA8ZyBpZD0iYW5nbGVkLXJpZ2h0Ij4gICAgICAgICAgICAgICAgPHJlY3QgaWQ9IlJlY3RhbmdsZSIgeD0iMCIgeT0iMCIgd2lkdGg9IjI0IiBoZWlnaHQ9IjI0Ij48L3JlY3Q+ICAgICAgICAgICAgICAgIDxwb2x5Z29uIGlkPSJpY29uIiBzdHJva2U9IiM5Nzk3OTciIGZpbGw9IiNGRkZGRkYiIHBvaW50cz0iNyAyIDIxIDEyIDcgMjIgNSAxOSAxNSAxMiA1IDUiPjwvcG9seWdvbj4gICAgICAgICAgICA8L2c+ICAgICAgICA8L2c+ICAgIDwvZz48L3N2Zz4="


btnLeft : String
btnLeft =
    "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz48c3ZnIHdpZHRoPSIxN3B4IiBoZWlnaHQ9IjIycHgiIHZpZXdCb3g9IjAgMCAxNyAyMiIgdmVyc2lvbj0iMS4xIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj4gICAgICAgIDx0aXRsZT5hbmdsZWQtbGVmdDwvdGl0bGU+ICAgIDxkZXNjPkNyZWF0ZWQgd2l0aCBTa2V0Y2guPC9kZXNjPiAgICA8ZGVmcz48L2RlZnM+ICAgIDxnIGlkPSJQYWdlLTEiIHN0cm9rZT0ibm9uZSIgc3Ryb2tlLXdpZHRoPSIxIiBmaWxsPSJub25lIiBmaWxsLXJ1bGU9ImV2ZW5vZGQiPiAgICAgICAgPGcgaWQ9IjI0IiB0cmFuc2Zvcm09InRyYW5zbGF0ZSgtNS4wMDAwMDAsIC0xLjAwMDAwMCkiPiAgICAgICAgICAgIDxnIGlkPSJhbmdsZWQtbGVmdCI+ICAgICAgICAgICAgICAgIDxyZWN0IGlkPSJSZWN0YW5nbGUiIHg9IjAiIHk9IjAiIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCI+PC9yZWN0PiAgICAgICAgICAgICAgICA8cG9seWdvbiBpZD0iaWNvbiIgc3Ryb2tlPSIjOTc5Nzk3IiBmaWxsPSIjRkZGRkZGIiB0cmFuc2Zvcm09InRyYW5zbGF0ZSgxMy4wMDAwMDAsIDEyLjAwMDAwMCkgc2NhbGUoLTEsIDEpIHRyYW5zbGF0ZSgtMTMuMDAwMDAwLCAtMTIuMDAwMDAwKSAiIHBvaW50cz0iNyAyIDIxIDEyIDcgMjIgNSAxOSAxNSAxMiA1IDUiPjwvcG9seWdvbj4gICAgICAgICAgICA8L2c+ICAgICAgICA8L2c+ICAgIDwvZz48L3N2Zz4="



-- Model


{-| -}
type Model
    = InternalState
        { current : Int
        , touch : Maybe ( Float, Float )
        , width : String
        , maybeElement : Maybe Browser.Dom.Element
        }



-- UPDATE


{-| -}
type Msg
    = Next (List String)
    | Prev (List String)
    | SetCurrent Int
    | StartAt ( Float, Float )
    | EndAt (List String) ( Float, Float )
    | Cancel ( Float, Float )
    | Other
    | GetElement (Result Browser.Dom.Error Browser.Dom.Element)
    | OnResize Int Int


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg (InternalState state) =
    let
        -- length =
        --     List.length state.images
        --
        prev list =
            clamp 0 (List.length list - 1) (state.current - 1)

        next list =
            clamp 0 (List.length list - 1) (state.current + 1)

        width =
            case state.maybeElement of
                Just element ->
                    element.element.width

                Nothing ->
                    1

        ( newState, cmd ) =
            case msg of
                Next images ->
                    ( { state | current = next images }, getElement )

                Prev images ->
                    ( { state | current = prev images }, getElement )

                SetCurrent index ->
                    ( { state | current = index }, getElement )

                StartAt coord ->
                    ( { state | touch = Just coord }, Cmd.none )

                EndAt images end ->
                    case state.touch of
                        Just start ->
                            if (Tuple.first end - Tuple.first start) / width > 0.2 then
                                ( { state | current = prev images, touch = Nothing }, Cmd.none )

                            else if Tuple.first end == Tuple.first start && width > 0 then
                                if (width - Tuple.first end) / width > 0.8 then
                                    ( { state | current = prev images, touch = Nothing }, Cmd.none )

                                else if (width - Tuple.first end) / width < 0.2 then
                                    ( { state | current = next images, touch = Nothing }, Cmd.none )

                                else
                                    ( state, Cmd.none )

                            else if (Tuple.first end - Tuple.first start) / width < -0.2 then
                                ( { state | current = next images, touch = Nothing }, Cmd.none )

                            else
                                ( { state | touch = Nothing }, Cmd.none )

                        Nothing ->
                            ( { state | touch = Nothing }, Cmd.none )

                Cancel _ ->
                    ( { state | touch = Nothing }, Cmd.none )

                Other ->
                    ( state, Cmd.none )

                GetElement result ->
                    ( { state | maybeElement = Result.toMaybe result }, Cmd.none )

                OnResize _ _ ->
                    ( state, getElement )
    in
    ( InternalState newState, cmd )



-- Cmd


getElement =
    Task.attempt GetElement (Browser.Dom.getElement "elm-slider")



-- VIEW


view : List String -> Model -> Html Msg
view images (InternalState { current, touch, width, maybeElement }) =
    div []
        [ div
            [ id "elm-slider"
            , class "overflow-hidden relative"
            , style "max-width" width
            , Touch.onStart (StartAt << touchCoordinates)
            , Touch.onEnd (EndAt images << touchCoordinates)
            , Touch.onCancel (Cancel << touchCoordinates)
            ]
            [ div
                [ style "transition" "all 0.5s ease-in"
                , style "transform" ("translateX(" ++ toPx (Maybe.map (\{ element } -> current * round element.width |> negate) maybeElement |> Maybe.withDefault 0) ++ ")")
                , style "touch-action" "none"
                , class "flex"
                ]
                (List.indexedMap (stage width current) images)
            , if current > 0 then
                div [ class "cursor-pointer h-full flex items-center pin-l pin-t absolute ", onClick <| Prev images ]
                    [ div [ class "w-8 text-right text-2xl md:text-4xl mx-2 md:mx-4" ] [ img [ src btnLeft ] [] ]
                    ]

              else
                text ""
            , if current + 1 < List.length images then
                div [ class "cursor-pointer h-full flex items-center pin-r pin-t absolute", onClick <| Next images ]
                    [ div [ class "w-8 text-left text-2xl md:text-4xl mx-2 md:mx-4" ] [ img [ src btnRight ] [] ]
                    ]

              else
                text ""
            ]
        , div [ style "max-width" width, class "flex justify-center" ] [ dots current images ]
        ]


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


stage : String -> Int -> Int -> String -> Html msg
stage width current index image =
    if image == "" then
        text ""

    else
        img
            [ src image
            , style "width" width
            , class "h-full flex-no-shrink min-w-0"
            ]
            []


toPx : Int -> String
toPx width =
    String.fromInt width ++ "px"


dots : Int -> List String -> Html Msg
dots current images =
    div
        [ class "flex justify-center mt-1 md:flex-wrap md:w-full md:justify-start"
        ]
        (List.indexedMap (dotsItem current) images)


dotsItem : Int -> Int -> String -> Html Msg
dotsItem current index image =
    let
        mobile =
            "rounded-full w-6 h-6 bg-color-black-light mx-1"

        ns =
            "md:rounded-none md:w-1/5 md:h-auto md:mx-0"

        active =
            "border border-solid border-black opacity-50 bg-black"

        nonactive =
            "opacity-100 border border-solid border-grey cursor-pointer"

        dot =
            "hidden md:block md:w-full md:h-auto"
    in
    if image == "" then
        text ""

    else if current == index then
        div
            [ class <| String.join " " [ mobile, ns, active ]
            ]
            [ img [ src image, class dot ] []
            ]

    else
        div
            [ onClick (SetCurrent index)
            , class <| String.join " " [ mobile, ns, nonactive ]
            ]
            [ img [ src image, class dot ] []
            ]



-- Init


init : Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok { width } ->
            ( initialModel width
            , getElement
            )

        Err _ ->
            ( initialModel "600"
            , Cmd.none
            )


initialModel : String -> Model
initialModel width =
    InternalState { current = 0, touch = Nothing, width = width, maybeElement = Nothing }



-- Subscriptions


subscriptions : List String -> Model -> Sub Msg
subscriptions images model =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder images)
        , Browser.Events.onResize
            OnResize
        ]


keyDecoder : List String -> Decoder Msg
keyDecoder list =
    Decode.map (toDirection list) (Decode.field "key" Decode.string)


toDirection : List String -> String -> Msg
toDirection list string =
    case string of
        "ArrowLeft" ->
            Prev list

        "ArrowRight" ->
            Next list

        _ ->
            Other



-- Flags


flagsDecoder : Decoder { width : String, images : List String }
flagsDecoder =
    Decode.map2 (\width images -> { width = width, images = images })
        (Decode.field "width" Decode.string)
        (Decode.field "images" (Decode.list Decode.string))



-- Main
-- main : Program Value Model Msg
-- main =
--     Browser.element
--         { init = init
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         }
