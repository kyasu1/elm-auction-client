module DragDrop exposing
    ( view
    , Config(..)
    , State, initialState
    , Msg, update
    )

{-| This library helps you create sortable elements by drag and drop.
Written by following _reusable views_ API design described in
[evancz/elm-sortable-table]: <https://github.com/evancz/elm-sortable-table>


# View

@docs view


# Configuration

@docs Config


# State

@docs State, initialState


# Update

@docs Msg, update

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


{-| Msg to hold the dragging state
-}
type Msg a
    = DragStart a
    | DragEnter a
    | DragLeave a
    | Drop a
    | DragEnd a
    | DragOver a



-- STATE


{-| Tracks dragging and hovering dom element.
-}
type alias State a =
    { dragging : Maybe a
    , hovering : Maybe a
    }


{-| Create a dragging state, no dom elmeent is selected at the beginning.
-}
initialState : State a
initialState =
    State Nothing Nothing



-- CONFIG


{-| **Note:** The `Config` should _never_ be held in your model.
It should only appear in `view` and `update` code.
-}
type Config a msg
    = Config
        { onDrop : a -> a -> msg
        , htmlTag : String
        , attributes : a -> List (Attribute (Msg a))
        , children : a -> Html (Msg a)
        }


{-| Create the `Config` for your `view` and `update` function.

    import DragDrop

    type Msg = DragDrop Image Image | ...

    config : DragDrop.Config Image Msg
    config =
      DragDrop.config
        { onDrop = DragDrop
        , htmlTag = "img"
        , attributes = (\image -> [("src", image.src)])
        , children = (\image -> div [] [text image.id])
        }

You provide the following infomation in you configuration:

  - `onDrop` &mdash; call back Msg that is called when drop event fired.
  - `htmlTag` &mdash; name of html tag to be draggable
  - `attributes` &mdash; list of extra attributes for the draggable element.
  - `children` &mdash; child nodes to be rendered in the draggable element.

-}
config :
    { onDrop : a -> a -> msg
    , htmlTag : String
    , attributes : a -> List (Attribute (Msg a))
    , children : a -> Html (Msg a)
    }
    -> Config a msg
config { onDrop, htmlTag, attributes, children } =
    Config
        { onDrop = onDrop
        , htmlTag = htmlTag
        , attributes = attributes
        , children = children
        }


{-| Update
-}
update : Config a msg -> Msg a -> State a -> ( State a, Maybe msg )
update (Config { onDrop }) msg model =
    case msg of
        DragStart dragged ->
            ( { model | dragging = Just dragged }, Nothing )

        DragEnter hovered ->
            ( { model | hovering = Just hovered }, Nothing )

        DragLeave _ ->
            ( { model | hovering = Nothing }, Nothing )

        DragOver _ ->
            ( model, Nothing )

        Drop dropped ->
            let
                updated =
                    { model | dragging = Nothing, hovering = Nothing }
            in
            case model.dragging of
                Just dragged ->
                    ( updated, Just <| onDrop dragged dropped )

                _ ->
                    ( updated, Nothing )

        DragEnd _ ->
            ( { model | dragging = Nothing, hovering = Nothing }, Nothing )



-- VIEW


{-| Accepts extra list of css styles to dynamicaly change the style and the model.
TODO: Probably should also accepts extra class for dynamic change.
-}
view : Config a msg -> List ( String, String ) -> a -> Html (Msg a)
view (Config { attributes, htmlTag, children }) style_ data =
    Html.node htmlTag
        (List.concat
            [ [ draggable "true"
              , onDragStart (DragStart data)
              , onDropHandler (Drop data)
              , onDragEnter (DragEnter data)
              , onDragOver (DragOver data)
              , onDragLeave (DragLeave data)
              , onDragEnd (DragEnd data)
              ]
            , List.map (\( k, v ) -> style k v) style_
            , attributes data
            ]
        )
        [ children data ]



-- EVENT HANDLERS


{-| Handles onenter dom event
-}
onDragEnter : msg -> Attribute msg
onDragEnter msg =
    on "dragenter" (Decode.succeed msg)


{-| Handles dragover dom event with preventDefault is True
-}
onDragOver : msg -> Attribute msg
onDragOver msg =
    onWithOptions "dragover" { stopPropagation = False, preventDefault = True } (Decode.succeed msg)


{-| Handles dragleave dom event
-}
onDragLeave : msg -> Attribute msg
onDragLeave msg =
    on "dragleave" (Decode.succeed msg)


{-| Handles dragstart dom event
-}
onDragStart : msg -> Attribute msg
onDragStart msg =
    on "dragstart" (Decode.succeed msg)


{-| Handles drop dom event
-}
onDropHandler : msg -> Attribute msg
onDropHandler msg =
    onWithOptions "drop" { stopPropagation = True, preventDefault = True } (Decode.succeed msg)


{-| Handles dragEnd dom event
-}
onDragEnd : msg -> Attribute msg
onDragEnd msg =
    on "dragend" (Decode.succeed msg)


onWithOptions : String -> { stopPropagation : Bool, preventDefault : Bool } -> Decode.Decoder msg -> Attribute msg
onWithOptions name { stopPropagation, preventDefault } decoder =
    decoder
        |> Decode.map (\msg -> { message = msg, stopPropagation = stopPropagation, preventDefault = preventDefault })
        |> custom name
