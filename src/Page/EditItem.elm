module Page.EditItem exposing (Model, Msg, initialEditTask, initialNewTask, update, view)

import Data.AuthToken exposing (AuthToken)
import Data.Item as Item exposing (Detail, Image, Item, ItemId)
import Data.Masters exposing (Masters)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Request.GraphQL.Error exposing (Error)
import Request.Item as Item
import Request.Masters as Masters
import Route
import Set
import Task exposing (Task)
import UI.Button as UI
import Util exposing (restLength, restSJISLength)
import Views.EditItem.Category as Category
import Views.EditItem.Condition as Condition
import Views.EditItem.Form as Form exposing (FormData, ItemForm, errorToString, toItem, validation)
import Views.EditItem.ImageUploader as ImageUploader
import Views.EditItem.PaymentMethod as PaymentMethod
import Views.EditItem.Selling as Selling
import Views.EditItem.Shipping as Shipping
import Views.Form as Form
import Views.Page as Page
import Views.Style as Style



-- MODEL --


type alias Model =
    { form : ItemForm
    , masters : Masters
    , uploader : ImageUploader.Model
    , category : Category.Model
    , loading : Bool
    , editting : Bool
    , itemId : Maybe ItemId
    , error : Maybe Error
    }


initialModel : Maybe ItemId -> Item Detail -> ( Masters, Session ) -> Category.Model -> ( Model, Session )
initialModel maybeItemId item ( masters, s2 ) category =
    ( { form = Form.initial (Form.initialize item) Form.validation
      , masters = masters
      , uploader = ImageUploader.initialModel item.details.images
      , category = category
      , loading = False
      , editting = False
      , itemId = maybeItemId
      , error = Nothing
      }
    , s2
    )


handleLoadError : a -> PageLoadError
handleLoadError err =
    pageLoadError Page.Other "申し訳ございません、商品編集ページが読み込めませんでした..."


initialEditTask : Session -> ItemId -> Task PageLoadError ( Model, Session )
initialEditTask session itemId =
    Item.get session itemId
        |> Task.andThen
            (\( item, updatedSession ) ->
                Task.map2 (initialModel (Just itemId) item)
                    (Masters.get updatedSession)
                    (Category.getModel updatedSession item.details.categoryId)
            )
        |> Task.mapError handleLoadError


initialNewTask : User a -> Session -> Task PageLoadError ( Model, Session )
initialNewTask user session =
    Task.map2 (initialModel Nothing (Item.initialItem user))
        (Masters.get session)
        (Category.getModel session 0)
        |> Task.mapError handleLoadError



-- initialTask : Session -> (Model, Session)
-- initialTask session item =
--     Task.map2 (initialModel maybeItemId item)
--       (Masters.get session)
--       (Category.getModel session item.details.categoryId)
--       |> Task.mapError handleLoadError
-- VIEW --


view : Model -> Html Msg
view model =
    let
        hasError =
            (Form.getErrors model.form |> List.length) > 0

        changed =
            Form.getChangedFields model.form |> Set.isEmpty |> not
    in
    section [ class "flex flex-col items-center justify-center" ] <|
        [ article [ class "" ]
            [ h2 [ class "text-center" ] [ text "商品の編集" ]
            , div [ classList [ ( "loading", model.loading ) ] ] []
            , div [] (formView model) |> Html.map FormMsg
            , Category.view model.category |> Html.map CategoryMsg
            , ImageUploader.view model.uploader |> Html.map ImageUploaderMsg

            -- , div [] [ Form.getErrors model.form |> Debug.toString |> text ]
            , div [ class "" ]
                [ button
                    [ type_ "submit"
                    , onClick Form.Submit
                    , class "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded"

                    -- , classList
                    --     [ ( Style.buttonH (not changed || hasError), True )
                    --     , ( "bg-silver white pointer", not hasError )
                    --     ]
                    ]
                    [ text "保存" ]
                    |> Html.map FormMsg
                , button
                    [ type_ "button"
                    , onClick Cancel
                    , classList
                        [ ( Style.button, True )
                        , ( "bg-silver white pointer", True )
                        ]
                    ]
                    [ text "キャンセル" ]
                ]
            ]
        ]


formView : Model -> List (Html Form.Msg)
formView ({ masters, form } as model) =
    [ UI.inputRest
        { label = "管理番号", required = False, name = "itemCode" }
        (restLength 20)
        errorToString
        (Form.getFieldAsString "itemCode" form)
    , UI.inputRest
        { label = "タイトル", required = True, name = "title" }
        (restSJISLength 65)
        errorToString
        (Form.getFieldAsString "title" form)
    , UI.inputRest { label = "キーワード", required = False, name = "keywords" }
        (restSJISLength 20)
        errorToString
        (Form.getFieldAsString "keywords" form)
    , UI.textarea
        { label = "商品詳細", required = True }
        (Form.getFieldAsString "description" form)
    , Html.node "auction-api-copy-text"
        [ Html.Attributes.attribute "data-copy-text"
            (Form.getFieldAsString "description" form |> .value |> Maybe.withDefault "")
        ]
        [ button [ class "bg-blue hover:bg-blue-dark text-white font-black py-2 px-4 rounded block mx-auto -mt-2" ] [ text "商品詳細をコピー" ]
        , Html.node "auction-api-copy-text-note" [] []
        ]
    , Selling.view model.form
    , PaymentMethod.view model
    , Shipping.form model.form
    , Condition.view model
    ]



-- UPDATE --


type Msg
    = SaveResponse (Result Error ( Item Detail, Session ))
      -- SaveResponse (WebData (Item Detail))
    | FormMsg Form.Msg
    | ImageUploaderMsg ImageUploader.Msg
    | CategoryMsg Category.Msg
      -- | SetCategory Category
    | Cancel


update : User AuthToken -> Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update user session msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just formData ) ->
                    let
                        newItem =
                            toItem formData user (ImageUploader.getImages model.uploader)
                    in
                    ( { model | loading = True }
                    , session
                    , Item.update session newItem |> Task.attempt SaveResponse
                    )

                _ ->
                    let
                        editting =
                            Form.getChangedFields model.form |> Set.isEmpty |> not
                    in
                    ( { model
                        | form = Form.update validation formMsg model.form
                        , editting = editting
                      }
                    , session
                    , Ports.sendInfoOutside <| Ports.SetEditState editting
                    )

        ImageUploaderMsg subMsg ->
            let
                ( newModel, subCmd, maybeImages ) =
                    ImageUploader.update subMsg model.uploader

                setField path val =
                    Form.Input path Form.Text <| Field.String val
            in
            case maybeImages of
                Just images ->
                    let
                        newForm =
                            List.indexedMap
                                (\index image ->
                                    let
                                        prefix =
                                            "images." ++ String.fromInt index
                                    in
                                    [ setField (prefix ++ ".data") image.data
                                    , setField (prefix ++ ".id") image.id
                                    , setField (prefix ++ ".itemId") image.itemId
                                    , setField (prefix ++ ".order") (String.fromInt image.order)
                                    ]
                                )
                                images
                                |> List.concat
                                |> List.foldl (\m form -> Form.update validation m form) model.form
                    in
                    ( { model | uploader = newModel, form = newForm }, session, Cmd.map ImageUploaderMsg subCmd )

                Nothing ->
                    ( { model | uploader = newModel }, session, Cmd.map ImageUploaderMsg subCmd )

        CategoryMsg subMsg ->
            let
                ( ( newModel, subCmd ), msgCategory ) =
                    Category.update session subMsg model.category

                setField path val =
                    Form.Input path Form.Text <| Field.String val

                newForm =
                    case msgCategory of
                        Category.NoOp ->
                            model.form

                        Category.SetCategory maybeCateogry ->
                            let
                                ( categoryId, categoryPath ) =
                                    case maybeCateogry of
                                        Just ( id, path ) ->
                                            ( String.fromInt id, path )

                                        Nothing ->
                                            ( "", "" )
                            in
                            List.foldl (\m form -> Form.update validation m form)
                                model.form
                                [ setField "categoryId" categoryId
                                , setField "categoryPath" categoryPath
                                ]
            in
            ( { model | category = newModel, form = newForm }, session, Cmd.map CategoryMsg subCmd )

        SaveResponse response ->
            case response of
                Ok ( item, updatedSession ) ->
                    ( { model | loading = False, editting = False }
                    , updatedSession
                    , Cmd.batch [ Ports.sendInfoOutside <| Ports.SetEditState False, Route.replaceUrl session.key (Route.Item item.id) ]
                    )

                -- TODO Handle case of Error
                Err error ->
                    ( { model | loading = False, error = Just error }, session, Cmd.none )

        Cancel ->
            ( { model | loading = False, editting = False }
            , session
            , Cmd.batch [ Ports.sendInfoOutside <| Ports.SetEditState False, Route.pushUrl session.key <| Route.Feed user.id ]
            )
