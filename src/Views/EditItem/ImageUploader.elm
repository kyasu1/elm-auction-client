module Views.EditItem.ImageUploader exposing
    ( Model
    , Msg(..)
    , getImages
    , initialModel
    , update
    , view
    )

import DragDrop
import File exposing (File)
import File.Select
import Html exposing (..)
import Html.Attributes as Attributes exposing (accept, class, classList, disabled, for, id, multiple, src, style, type_)
import Html.Events exposing (on, onClick)
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import UI.Button as UI
import UI.Svg exposing (noImage)



--main : Program Never Model Msg
--main =
--    Html.program
--        { init = ( initialModel images, Cmd.none )
--        , update = update
--        , view = view
--        , subscriptions = (always Sub.none)
--        }


initialModel : List Image -> Model
initialModel images =
    Model
        { message = "Waiting..."
        , images = images
        , selected = Nothing
        , dragDropState = DragDrop.initialState
        }


maxImages : Int
maxImages =
    10


initialImages : List Image
initialImages =
    List.map (\order -> Image (String.fromInt order) order "" "") <| List.range 0 (maxImages - 1)


idSingleImage : Int -> String
idSingleImage order =
    "id-single-image-" ++ String.fromInt order


idMultipleImages : String
idMultipleImages =
    "id-multiple-images"



-- MODEL


type Model
    = Model InternalModel


type alias ImagePortData =
    { contents : String
    , filename : String
    }


type alias Image =
    { id : String
    , order : Int
    , data : String
    , itemId : String
    }


type alias InternalModel =
    { message : String
    , images : List Image
    , selected : Maybe Image
    , dragDropState : DragDrop.State Image
    }



-- PUBLIC FUNCTIONS --


getImages : Model -> List Image
getImages (Model model) =
    model.images



-- UPDATE


type Msg
    = ImageSelected (Maybe Image)
    | Clear Image
    | ImageLoaded File
    | ImageConverted String
    | ImagesLoaded File (List File)
    | ImagesConverted (List String)
    | DragDrop Image Image
    | DragDropMsg (DragDrop.Msg Image)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe (List Image) )
update msg (Model model) =
    let
        ( internalModel, updatedMsg, images ) =
            updateInternal msg model
    in
    ( Model internalModel, updatedMsg, images )


updateInternal : Msg -> InternalModel -> ( InternalModel, Cmd Msg, Maybe (List Image) )
updateInternal msg model =
    case msg of
        ImageSelected maybeImage ->
            case maybeImage of
                Nothing ->
                    ( { model | selected = Nothing }
                      -- , PortMessage.new "FileSelected"
                      --     |> PortMessage.withPayload (Encode.string idMultipleImages)
                      --     |> PortMessage.elmToJs
                    , File.Select.files [ "image/jpg" ] ImagesLoaded
                    , Nothing
                    )

                Just image ->
                    ( { model | selected = Just image }
                      -- , PortMessage.new "FileSelected"
                      --     |> PortMessage.withPayload (Encode.string <| idSingleImage image.order)
                      --     |> PortMessage.elmToJs
                    , File.Select.file [ "image/jpg" ] ImageLoaded
                    , Nothing
                    )

        ImagesLoaded file listOfFile ->
            ( model
            , file
                :: listOfFile
                |> List.map File.toUrl
                |> Task.sequence
                |> Task.perform ImagesConverted
            , Nothing
            )

        ImagesConverted images ->
            let
                newImages =
                    let
                        length =
                            List.length images

                        empty =
                            List.range length (maxImages - 1) |> List.map (\_ -> "")

                        updateImage : Int -> String -> Image -> Image
                        updateImage order data image =
                            { image | order = order, data = data }
                    in
                    List.map3
                        updateImage
                        (List.range 1 maxImages)
                        (List.take maxImages images ++ empty)
                        model.images
            in
            ( { model | images = newImages, selected = Nothing }, Cmd.none, Just newImages )

        ImageLoaded file ->
            ( model
            , file
                |> File.toUrl
                |> Task.perform ImageConverted
            , Nothing
            )

        ImageConverted converted ->
            case model.selected of
                Just selected ->
                    let
                        newImages =
                            let
                                updated selectedImage image =
                                    if selectedImage.order == image.order then
                                        { image | data = converted }

                                    else
                                        image
                            in
                            List.map (updated selected) model.images
                    in
                    ( { model | images = newImages, selected = Nothing }, Cmd.none, Just newImages )

                Nothing ->
                    ( model, Cmd.none, Nothing )

        {--OLD CODDE
        ImageRead value ->
            let
                images =
                    decodeImage value

                newImages =
                    case model.selected of
                        {- multiple files are selected -}
                        Nothing ->
                            let
                                length =
                                    List.length images

                                empty =
                                    List.range length (maxImages - 1) |> List.map (\_ -> "")

                                updateImage : Int -> String -> Image -> Image
                                updateImage order data image =
                                    { image | order = order, data = data }
                            in
                            List.map3
                                updateImage
                                (List.range 1 maxImages)
                                (List.take maxImages images ++ empty)
                                model.images

                        {- Single file is selected -}
                        Just selected ->
                            let
                                updated selectedImage image =
                                    if selectedImage.order == image.order then
                                        { image | data = images |> List.head |> Maybe.withDefault "" }

                                    else
                                        image
                            in
                            List.map (updated selected) model.images
            in
            ( { model | images = newImages, selected = Nothing }, Cmd.none, Just newImages )
--}
        Clear selected ->
            let
                newImages =
                    let
                        updated imageSelected image =
                            if imageSelected.order == image.order then
                                { image | data = "" }

                            else
                                image
                    in
                    List.map (updated selected) model.images
            in
            ( { model | images = newImages, selected = Nothing }, Cmd.none, Just newImages )

        DragDropMsg childMsg ->
            let
                ( updated, maybeMsg ) =
                    DragDrop.update config childMsg model.dragDropState

                newModel =
                    { model | dragDropState = updated }
            in
            case maybeMsg of
                Nothing ->
                    ( newModel, Cmd.none, Nothing )

                Just updateMsg ->
                    updateInternal updateMsg newModel

        DragDrop dragged hovered ->
            let
                swapped =
                    List.map (swapOrder dragged.order hovered.order) model.images
            in
            ( { model | images = swapped }, Cmd.none, Just swapped )


swapOrder : Int -> Int -> Image -> Image
swapOrder draggedOrder hoveredOrder image =
    if image.order == draggedOrder then
        { image | order = hoveredOrder }

    else if image.order == hoveredOrder then
        { image | order = draggedOrder }

    else
        image



-- portDataDecoder : Decode.Decoder (List String)
-- portDataDecoder =
--     Decode.list <|
--         Decode.field "contents" Decode.string
--
--
-- decodeImage : Decode.Value -> List String
-- decodeImage data =
--     case Decode.decodeValue portDataDecoder data of
--         Ok src ->
--             src
--
--         Err _ ->
--             []
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ label [ class "font-black block m-4 p-2" ] [ text "出品画像" ]
        , preview model
        , div [ class "text-center" ]
            [ UI.darkButton { label = "複数の画像を登録", icon = "fas fa-file-upload" } (ImageSelected Nothing) ]
        ]


preview : Model -> Html Msg
preview ((Model { images }) as model) =
    let
        sorted =
            List.sortBy (\image -> image.order) images
    in
    Html.Keyed.ul [ class "list-reset flex flex-wrap" ]
        (List.map (\item -> ( item.id, imageView model item )) sorted)


imageView : Model -> Image -> Html Msg
imageView (Model { dragDropState }) image =
    let
        imageStyle =
            [ ( "cursor", "move" )
            ]

        draggedStyle =
            [ ( "opacity", "0.4" ) ]

        hoveredStyle =
            [ ( "border", "1px dashed #555" ) ]

        dragDropStyle =
            case ( dragDropState.dragging, dragDropState.hovering ) of
                ( Just dragged, Just hovered ) ->
                    if image.id == dragged.id then
                        draggedStyle

                    else if image.id == hovered.id then
                        hoveredStyle

                    else
                        []

                ( Just dragged, Nothing ) ->
                    if image.id == dragged.id then
                        draggedStyle

                    else
                        []

                _ ->
                    []

        style_ =
            imageStyle ++ dragDropStyle

        fileUploadId =
            "file-upload-" ++ image.id
    in
    li [ class "w-50 md:w-1/5" ]
        [ Html.map DragDropMsg (DragDrop.view config style_ image)
        , div [ class "flex" ]
            [ div [ class "w-1/2 p-2" ]
                [ case image.data /= "" of
                    True ->
                        UI.darkButton { label = "削除", icon = "fas fa-trash-alt" } (Clear image)

                    False ->
                        UI.darkButtonDisabled { label = "削除", icon = "fas fa-trash-alt" } (Clear image)
                ]
            , div [ class "w-1/2 p-2" ]
                [ UI.darkButton { label = "登録", icon = "fas fa-file-upload" } (ImageSelected (Just image)) ]

            -- , div [ class "w-1/2 px-2" ]
            --     [ label
            --         [ class "text-white bg-grey-darkest ml-2 inline-block" ]
            --         [ i [ class "fas fa-file-upload p-1 border-r" ] []
            --         , span [ class "p-1" ] [ text "登録" ]
            --         , input
            --             [ type_ "file"
            --             , accept "image/jpeg"
            --             , class "hidden"
            --             , on "change" (Decode.succeed (ImageSelected (Just image)))
            --             , id <| idSingleImage image.order
            --             ]
            --             []
            --         ]
            --     ]
            ]
        ]


config : DragDrop.Config Image Msg
config =
    DragDrop.Config
        { onDrop = DragDrop
        , htmlTag = "div"
        , attributes = attrHelper
        , children = childHelper
        }


attrHelper : Image -> List (Html.Attribute msg)
attrHelper image =
    [ class "w-full p-2" ]


childHelper : Image -> Html msg
childHelper image =
    if image.data == "" then
        noImage

    else
        img [ src image.data, class "border" ] []
