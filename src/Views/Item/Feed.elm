module Views.Item.Feed exposing (Model, Msg, initialTask, subscriptions, update, view)

import Data.Item as Item exposing (Item, ItemId)
import Data.Item.Feed as Feed exposing (Feed)
import Data.Selling as Selling
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User, UserId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import List.Selection as Selection
import Request.Download
import Request.GraphQL.Error exposing (Error)
import Request.Item as Item exposing (ListConfig, list)
import Request.User as User
import Route exposing (pushUrl, replaceUrl)
import SelectList as SelectList
import Task exposing (Task)
import Util
import Views.Dialog as Dialog
import Views.Helper exposing (toShortString, toYenPrice)
import Views.Style as Style


type Model
    = Model InternalModel


type alias InternalModel =
    { errors : List String
    , feed : Feed
    , dialog : Maybe (Html Msg)
    , query : String
    , userList : Selection.Selection SelectList.ListItem
    }


initialModel : ListConfig -> ( Feed, Session ) -> List (User ()) -> ( Model, Session )
initialModel config ( feed, session ) userList =
    ( Model
        { errors = []
        , feed = feed
        , dialog = Nothing
        , query = config.query |> Maybe.withDefault ""
        , userList = SelectList.init userList
        }
    , session
    )


initialTask : Session -> ListConfig -> Task Error ( Model, Session )
initialTask session config =
    Task.map2 (initialModel config) (Feed.list config session) User.list


view : Session -> Model -> Html Msg
view session (Model model) =
    article [ class "w-full max-w-xl" ]
        [ searchView model
        , menuView session model
        , hr [ class "mb-1" ] []
        , if List.length model.feed.items > 0 then
            ul [ class "list-reset" ] (model.feed.items |> List.map (itemView session model))

          else
            emptyView model.query

        -- , hr [] []
        , pageControlView model.feed
        , Dialog.view model.dialog
        ]


emptyView : String -> Html msg
emptyView query =
    div [ class "flex flex-column items-center justify-center" ]
        [ hr [] []
        , div [ class "flex-auto" ] [ text query, text "に関する結果はありません。" ]
        , hr [] []
        ]


checkBoxWithIcon : Bool -> msg -> Html msg
checkBoxWithIcon selected msg =
    let
        icon =
            case selected of
                True ->
                    i [ class "far fa-check-square" ] []

                False ->
                    i [ class "far fa-square" ] []
    in
    label [ class "px-2 text-lg cursor-pointer" ]
        [ icon
        , input
            [ type_ "checkbox"
            , onClick msg
            , class "hidden"
            ]
            []
        ]


itemView : Session -> InternalModel -> { item : Item (), selected : Bool } -> Html Msg
itemView session model { item, selected } =
    let
        url =
            if item.thumb == "" then
                "/images/no_image.png"

            else
                item.thumb
    in
    li
        [ classList [ ( "flex border-b border-grey items-center", True ), ( "bg-yellow-light", selected ) ]
        ]
        [ checkBoxWithIcon selected (RowSelected item.id)
        , checkBoxWithIcon selected (SetPublished item.id)
        , img [ src url, class "w-24 h-24" ] []
        , div [ class "flex flex-col px-2 py-2 justify-between w-full h-24" ]
            [ a [ class "ml-2 no-underline text-black", Route.href (Route.Item item.id) ] [ text item.title ]
            , div [ class "flex flex-col md:flex-row justify-between" ]
                [ buttonsView session model item
                , div [ class "flex pb-2" ]
                    [ div [ class "pl-1" ]
                        [ text (price item.selling)
                        ]
                    , div [ class "pl-1" ]
                        [ text item.userName
                        ]
                    , div [ class "pl-1" ]
                        [ item.updatedAt |> toShortString |> text
                        ]
                    ]
                ]
            ]
        ]


price : Selling.Selling -> String
price selling =
    case selling of
        Selling.Auction priceOpen priceBuyout ->
            case priceBuyout of
                0 ->
                    (priceOpen |> toYenPrice) ++ "スタート"

                _ ->
                    (priceOpen |> toYenPrice) ++ "スタート" ++ (priceBuyout |> toYenPrice) ++ "円即決"

        Selling.Fixed priceBuyout allowDiscount ->
            (priceBuyout |> toYenPrice) ++ "(税込" ++ (Util.addTax008 priceBuyout |> toYenPrice) ++ ")"


buttonsView : Session -> InternalModel -> Item () -> Html Msg
buttonsView session model item =
    let
        buttonEdit =
            darkLink { label = "編集", icon = "fas fa-edit" } (Route.EditItem item.id)

        buttonDelete =
            darkButton { label = "削除", icon = "fas fa-trash-alt" } (DeleteItem item)

        buttonDuplicate =
            darkButton { label = "複製", icon = "fas fa-copy" } (CopyItem item.id)

        buttonDownload =
            darkButton { label = "ダウンロード", icon = "fas fa-download" } (DownloadRequestSingle item.id)
    in
    div [ class "flex" ]
        (case session.user of
            Just user ->
                if user.id == item.userId then
                    [ div [] [ buttonEdit ]
                    , div [] [ buttonDelete ]
                    , div [] [ buttonDuplicate ]
                    , div [] [ buttonDownload ]
                    ]

                else
                    [ div [] [ buttonDuplicate ]
                    , div [] [ buttonDownload ]
                    ]

            Nothing ->
                []
        )


searchView : InternalModel -> Html Msg
searchView model =
    Html.form [ class "flex py-4", onSubmit SearchSubmit ]
        [ input
            [ type_ "text"
            , class "border p-2 w-full"
            , placeholder "キーワード検索"
            , onInput SearchInput

            --            , defaultValue model.query
            ]
            []
        , input
            [ type_ "submit"
            , class "border-2 border-black p-2 ml-2 hover:bg-grey"
            , value "検\u{3000}索"
            ]
            [ text "キーワード検索"
            ]
        ]


menuView : Session -> InternalModel -> Html Msg
menuView session model =
    let
        count =
            List.length model.feed.items

        selected =
            model.feed.items |> List.filter (\e -> e.selected == True) |> List.length

        selectAll =
            let
                ( icon, msg ) =
                    if selected == 0 then
                        ( "far fa-square", SelectAll )

                    else if selected == count then
                        ( "far fa-check-square", DeselectAll )

                    else
                        ( "far fa-minus-square", DeselectAll )
            in
            [ label [ class "dib self-center mh2" ]
                [ i [ class icon ] []
                , input [ type_ "checkbox", onClick msg, class "hidden" ] []
                ]
            ]

        refresh =
            [ darkButton { label = "更新", icon = "fas fa-sync-alt" } RefreshClicked
            ]

        download =
            if Feed.downlodable model.feed then
                [ darkButton { label = "まとめてダウンロード", icon = "fas fa-download" } DownloadRequestMultiple
                ]

            else
                []

        delete =
            if Feed.deletable session model.feed then
                [ darkButton { label = "まとめて削除", icon = "fas fa-trash-alt" } DeleteMultiple
                ]

            else
                []

        terms =
            [ select [ class "input-reset f6 ba pa2 mr2" ]
                [ option [ value "all" ] [ text "全商品区分" ]
                , option [ value "watch" ] [ text "時計" ]
                , option [ value "jewelry" ] [ text "ジュエリー" ]
                , option [ value "leather" ] [ text "バッグ/小物" ]
                ]
            ]
    in
    case session.user of
        Just user ->
            div [ class "flex" ]
                (List.concat
                    [ selectAll
                    , refresh
                    , download
                    , delete
                    , terms
                    , [ SelectList.view ChangeUser model.feed.userId model.userList ]
                    ]
                )

        Nothing ->
            div [ class "flex" ]
                (List.concat [ refresh, terms ])


darkLink : { label : String, icon : String } -> Route.Route -> Html msg
darkLink { label, icon } route =
    a
        [ class "block no-underline text-white bg-grey-darkest ml-2"
        , Route.href route
        ]
        [ i [ class <| String.join " " [ icon, "p-1 border-r" ] ] []
        , span [ class "p-1" ] [ text label ]
        ]


darkButton : { label : String, icon : String } -> msg -> Html msg
darkButton { label, icon } msg =
    button
        [ class "text-white bg-grey-darkest ml-2"
        , onClick msg
        ]
        [ i [ class <| String.join " " [ icon, "p-1 border-r" ] ] []
        , span [ class "p-1" ] [ text label ]
        ]


showHide : Bool -> String
showHide cond =
    if cond then
        "f6 link br2 ph2 pv1 dim tc pointer black bg-silver dib"

    else
        "f6 link br2 ph2 pv1 tc white bg-silver dn"


pageControlView : Feed -> Html Msg
pageControlView feed =
    if feed.pageInfo.hasNextPage then
        div [ class "text-center my-2" ]
            [ button
                [ onClick LoadMore
                , class "p-2 border-2 border-black text-black hover:grey"
                ]
                [ text "さらに読み込み" ]
            ]

    else
        text ""


downloadDialog : Maybe String -> Html Msg
downloadDialog maybeFileName =
    div []
        [ case maybeFileName of
            Just fileName ->
                a
                    [ href <| "/download?filename=" ++ fileName
                    , Html.Attributes.download fileName
                    , onClick CloseDialog
                    ]
                    [ text "ダウンロード" ]

            Nothing ->
                span [] [ text "ダウンロードの準備中" ]
        ]


deleteDialog : List (Item ()) -> Html Msg
deleteDialog listOfItem =
    Dialog.yesNoDialog "商品データの削除"
        (div [ class "f6" ]
            [ p [] [ text "下記の商品データは削除され編集する事ができなくなります" ]
            , div [ class "" ] (List.map (\e -> div [ class "truncate" ] [ text e.title ]) listOfItem)
            ]
        )
        { message = DeleteExecute listOfItem
        , label = "削除する"
        }
        { message = CloseDialog
        , label = "削除しない"
        }



-- div [ class "flex" ]
--     [ a [ onClick First, class (showHide (Paginate.isFirst feed)) ] [ text "<<" ]
--     , a [ onClick Prev, class (showHide (Paginate.isFirst feed)) ] [ text "<" ]
--     , div [ class "flex-auto tc" ]
--         [ span [] [ text (Paginate.length feed |> toString) ]
--         , span [] [ text "件" ]
--         , span [] [ text (Paginate.currentPage feed |> toString) ]
--         , span [] [ text " / " ]
--         , span [] [ text (Paginate.totalPages feed |> toString) ]
--         ]
--     , a [ onClick Next, class (showHide (Paginate.isLast feed)) ] [ text ">" ]
--     , a [ onClick Last, class (showHide (Paginate.isLast feed)) ] [ text ">>" ]
--     ]
-- UPDATE --


type Msg
    = RowSelected ItemId
    | SetPublished ItemId
    | ViewItem ItemId
    | EditItem ItemId
    | DeleteItem (Item ())
    | DeleteMultiple
    | DeleteExecute (List (Item ()))
    | DeleteResponse (List (Item ())) (Result Error ( List ItemId, Session ))
    | CopyItem ItemId
    | CopyResponse ItemId (Result Error ( Item (), Session ))
    | LoadMore
    | LoadResponse (Result Error ( Feed, Session ))
    | SearchSubmit
    | SearchInput String
    | SearchResponse (Result Error ( Feed, Session ))
    | SelectAll
    | DeselectAll
    | RefreshClicked
    | DownloadRequestSingle ItemId
    | DownloadRequestMultiple
    | DownloadReady (Result Error ( String, Session ))
    | CloseDialog
    | NoOp
    | ChangeUser String


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update session msg (Model internalModel) =
    let
        ( model, updatedSession, cmd ) =
            updateInternal session msg internalModel
    in
    ( Model model, updatedSession, cmd )


updateInternal : Session -> Msg -> InternalModel -> ( InternalModel, Session, Cmd Msg )
updateInternal session msg model =
    case msg of
        RowSelected itemId ->
            ( { model | feed = Feed.setSelected itemId model.feed }, session, Cmd.none )

        SetPublished itemdId ->
            ( model, session, Cmd.none )

        ViewItem itemId ->
            ( model, session, pushUrl session.key (Route.Item itemId) )

        EditItem itemId ->
            ( model, session, pushUrl session.key (Route.EditItem itemId) )

        CopyItem itemId ->
            ( { model | dialog = Just (Dialog.loadingDialog "複製作成中...") }, session, cmdCopy itemId session )

        CopyResponse itemId response ->
            case response of
                Ok ( item, updatedSession ) ->
                    ( { model | feed = Feed.prependItem item model.feed, dialog = Nothing }, updatedSession, Cmd.none )

                Err error ->
                    ( { model | dialog = Just (Dialog.closableDialog "複製できませでした" CloseDialog) }, session, Cmd.none )

        DeleteItem item ->
            ( { model | dialog = Just (deleteDialog [ item ]) }, session, Cmd.none )

        DeleteMultiple ->
            ( { model | dialog = Just (model.feed |> Feed.getSelected |> deleteDialog) }, session, Cmd.none )

        DeleteExecute listOfItem ->
            ( { model | dialog = Just (Dialog.loadingDialog "削除処理中...") }, session, cmdDelete listOfItem session )

        DeleteResponse listOfItem response ->
            case response of
                Ok ( _, updatedSession ) ->
                    ( { model | dialog = Nothing, feed = Feed.removeMultiple listOfItem model.feed }, updatedSession, Cmd.none )

                Err error ->
                    ( { model | dialog = Just (Dialog.closableDialog "削除処理中にエラーが発生しました" CloseDialog) }, session, Cmd.none )

        LoadMore ->
            ( model
            , session
            , Feed.more session model.feed
                |> Task.attempt LoadResponse
            )

        LoadResponse response ->
            case response of
                Ok ( feed, updatedSession ) ->
                    ( { model
                        | feed = Feed.append model.feed feed
                      }
                    , updatedSession
                    , Cmd.none
                    )

                Err err ->
                    ( model, session, Cmd.none )

        -- Search related
        SearchSubmit ->
            if model.query /= "" then
                ( model, session, pushUrl session.key (Route.Search (Just model.query)) )

            else
                ( model, session, Cmd.none )

        SearchResponse response ->
            case response of
                Ok ( feed, updatedSession ) ->
                    ( { model
                        | feed = feed
                      }
                    , updatedSession
                    , Cmd.none
                    )

                Err err ->
                    ( model, session, Cmd.none )

        SearchInput query ->
            ( { model | query = query }, session, Cmd.none )

        SelectAll ->
            ( { model | feed = Feed.selectAll model.feed }, session, Cmd.none )

        DeselectAll ->
            ( { model | feed = Feed.deselectAll model.feed }, session, Cmd.none )

        RefreshClicked ->
            ( model, session, Cmd.none )

        -- File download related
        DownloadRequestSingle itemId ->
            ( { model | dialog = Just (downloadDialog Nothing) }
            , session
            , Request.Download.download session [ itemId ]
                |> Task.attempt DownloadReady
            )

        DownloadRequestMultiple ->
            ( { model | dialog = Just (downloadDialog Nothing) }
            , session
            , Request.Download.download session (Feed.getSelected model.feed |> List.map .id)
                |> Task.attempt DownloadReady
            )

        DownloadReady (Ok ( filename, updatedSession )) ->
            ( { model | dialog = Just (downloadDialog (Just filename)) }, updatedSession, Cmd.none )

        DownloadReady (Err error) ->
            ( { model | dialog = Just (Dialog.closableDialog "ダウンロードの準備中にエラーが発生しました" CloseDialog) }, session, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, session, Cmd.none )

        NoOp ->
            ( model, session, Cmd.none )

        ChangeUser value ->
            let
                route =
                    if value == "" then
                        Route.Home

                    else
                        Route.Feed (User.stringToUserId value)
            in
            ( { model | userList = SelectList.select value model.userList }
            , session
            , pushUrl session.key route
            )



-- COMMANDS --


cmdCopy : ItemId -> Session -> Cmd Msg
cmdCopy itemId session =
    case session.user of
        Just user ->
            Feed.copy itemId session
                |> Task.attempt (CopyResponse itemId)

        Nothing ->
            Cmd.none


cmdDelete : List (Item ()) -> Session -> Cmd Msg
cmdDelete listOfItem session =
    case session.user of
        Just user ->
            Feed.deleteMultiple listOfItem session
                |> Task.attempt (DeleteResponse listOfItem)

        --            Feed.delete itemId session
        --                |> Http.send (DeleteResponse itemId)
        Nothing ->
            Cmd.none



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    case model.dialog of
        Just dialog ->
            Dialog.subscriptions { close = CloseDialog, none = NoOp }

        Nothing ->
            Sub.none
