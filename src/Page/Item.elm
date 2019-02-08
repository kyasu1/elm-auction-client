module Page.Item exposing (Model, Msg, initialTask, update, view)

{-| Viewing an individual item by registered users
-}

import Data.Condition as Condition
import Data.Item as Item exposing (Detail, Item, ItemId)
import Data.Masters as Masters exposing (Masters)
import Data.Selling as Selling
import Data.Session as Session exposing (Session)
import Data.Shipping as Shipping
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Download
import Request.GraphQL.Error exposing (Error)
import Request.Item as Item
import Request.Masters as Masters
import Route
import Slider
import Task exposing (Task)
import Util
import Views.Dialog as Dialog
import Views.Helper exposing (toLongString, toYenPrice)
import Views.Page as Page
import Views.Style as Style



-- MODEL --


type alias Model =
    { item : Item Detail
    , slider : Slider.Model
    , masters : Masters
    , dialog : Dialog.Dialog Msg
    }


initialModel : ( Masters, Session ) -> ( Item Detail, Session ) -> ( Model, Session )
initialModel ( masters, s1 ) ( item, s2 ) =
    ( { item = item
      , slider = Slider.initialModel "1200px" (List.map .data item.details.images)
      , masters = masters
      , dialog = Nothing
      }
    , s2
    )


handleLoadError : a -> PageLoadError
handleLoadError err =
    pageLoadError Page.Other "申し訳ございません、商品ページが読み込めませんでした..."


initialTask : Session -> ItemId -> Task PageLoadError ( Model, Session )
initialTask session itemId =
    let
        maybeToken =
            Maybe.map .token session.user
    in
    Task.map2 initialModel
        (Masters.get session)
        (Item.get session itemId)
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    section [ class "flex items-center justify-center flex-column flex-auto" ] <|
        [ article [ class "mw5-ns bl-ns br-ns pv3-ns ph4-ns  b--light-gray w-90 mw8-ns" ]
            [ itemView session model model.item
            ]
        , Dialog.view model.dialog
        ]


itemView : Session -> Model -> Item Detail -> Html Msg
itemView session model item =
    div [ class "" ]
        [ h4 [ class "f6 normal" ] [ text item.details.categoryPath ]
        , h3 [ class "f5 b" ] [ text item.title ]
        , div [ class "flex flex-column" ]
            [ div [ class "flex flex-column flex-row-ns" ]
                [ imageView model item
                , detailsView model
                ]
            ]
        , div [ class "ba-ns ph3-ns pb3-ns mt1 f6" ]
            [ Markdown.toHtml [] item.details.description ]
        , Html.node "auction-api-copy-text"
            [ Html.Attributes.attribute "data-copy-text" item.details.description
            ]
            [ button [ class "bg-blue hover:bg-blue-dark text-white font-black py-2 px-4 rounded block mx-auto -mt-2" ] [ text "商品詳細をコピー" ]
            , Html.node "auction-api-copy-text-note" [] []
            ]
        , buttonsView session model item
        ]


imageView : Model -> Item Detail -> Html Msg
imageView { slider } item =
    let
        imageList =
            item.details.images
                |> List.sortBy .order
                |> List.map
                    (\image ->
                        if image.data == "" then
                            "/images/no_image.png"
                            -- Assets.env Assets.noImage

                        else
                            image.data
                    )
    in
    div [ class "w-100 w-40-m w-50-l flex-shrink-0" ]
        [ Slider.view slider |> Html.map SliderMsg
        ]


detailsView : Model -> Html Msg
detailsView { item, masters } =
    let
        styleTh =
            "fw6 bb b--black-20 pv2 pr3 bg-white tr"

        styleTd =
            "    bb b--black-20 pv2 pl3 bg-white tl"

        row ( l, v ) =
            tr []
                [ th [ class styleTh ] [ text l ]
                , td [ class styleTd ] [ text v ]
                ]

        shipping =
            case item.details.shipping of
                Shipping.Seller ->
                    [ ( "送料負担", Shipping.fromPayer Shipping.Seller ) ]

                Shipping.Buyer method ->
                    case method of
                        Shipping.Takkyubin size ->
                            [ ( "送料負担", Shipping.fromPayer <| Shipping.Buyer method )
                            , ( "発送方法", Shipping.fromMethod <| Shipping.Takkyubin size )
                            , ( "サイズ", Shipping.fromSize <| size )
                            ]

                        _ ->
                            [ ( "送料負担", Shipping.fromPayer <| Shipping.Buyer method )
                            , ( "発送方法", Shipping.fromMethod method )
                            ]

        selling =
            case item.selling of
                Selling.Auction priceOpen priceBuyout ->
                    case priceBuyout of
                        0 ->
                            [ ( "販売方法", "オークション形式" )
                            , ( "開始価格", toYenPrice priceOpen )
                            ]

                        _ ->
                            [ ( "販売方法", "オークション形式" )
                            , ( "開始価格", toYenPrice priceOpen )
                            , ( "即決価格", toYenPrice priceBuyout ++ "(税込" ++ (Util.addTax008 priceBuyout |> toYenPrice) ++ ")" )
                            ]

                Selling.Fixed priceBuyout allowDiscount ->
                    [ ( "販売方法", "即決形式" )
                    , ( "即決価格", toYenPrice priceBuyout ++ "(税込" ++ (Util.addTax008 priceBuyout |> toYenPrice) ++ ")" )
                    , ( "値下交渉"
                      , if allowDiscount == True then
                            "あり"

                        else
                            "無し"
                      )
                    ]

        topPart =
            [ ( "商品の状態", Masters.lookupText "conditions" (Condition.toString item.details.condition) masters |> Maybe.withDefault "" )
            , ( "商品の状態備考", item.details.conditionDetail )
            , ( "支払方法", Masters.lookupText "payment_methods" item.details.paymentMethod masters |> Maybe.withDefault "" )
            ]

        bottomPart =
            [ ( "管理番号", item.details.itemCode ), ( "更新日", toLongString item.updatedAt ) ]
    in
    div [ class "w-100 ph1 ph3-ns" ]
        [ table [ class "f6 w-100 center mw8 collapse" ]
            [ tbody
                []
                (List.map row <| List.concat [ topPart, selling, shipping, bottomPart ])
            ]
        ]


buttonsView : Session -> Model -> Item Detail -> Html Msg
buttonsView session model item =
    let
        buttonEdit =
            a
                [ classList
                    [ ( Style.buttonSmall, True )
                    , ( "link white bg-silver", True )
                    ]

                -- , onClick (EditItem item.id)
                , Route.href (Route.EditItem item.id)
                ]
                [ text "編集" ]

        buttonDelete =
            a
                [ classList
                    [ ( Style.buttonSmall, True )
                    , ( "link white bg-dark-red", True )
                    ]
                , onClick Delete
                ]
                [ text "削除" ]

        buttonDownload =
            button
                [ classList
                    [ ( Style.buttonBase, True )
                    , ( Style.buttonSmall, True )
                    , ( "white bg-silver flex items-center", True )
                    ]
                , onClick PrepareDownload
                ]
                [ i [ class "material-icons f5 mr1" ] [ text "file_download" ]
                , span [] [ text "ダウンロード" ]
                ]
    in
    div []
        [ ul [ class "flex list pl0 mt2" ]
            (case session.user of
                Just user ->
                    if user.id == item.userId then
                        [ li [ class "ph1" ] [ buttonEdit ]
                        , li [ class "ph1" ] [ buttonDelete ]
                        , li [ class "ph1" ] [ buttonDownload ]
                        ]

                    else
                        [ li [ class "ph1" ] [ buttonDownload ]
                        ]

                Nothing ->
                    [ text "" ]
            )
        ]


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


deleteDialog : Html Msg
deleteDialog =
    Dialog.yesNoDialog "商品データの削除"
        (p [] [ text "商品データは削除され編集することができなくなります" ])
        { message = DeleteExecute
        , label = "削除する"
        }
        { message = CloseDialog
        , label = "削除しない"
        }



-- UPDATE --


type Msg
    = -- EditItem ItemId
      PrepareDownload
    | FileDownloaded
    | FileReady (Result Error ( String, Session ))
    | Delete
    | DeleteExecute
    | DeleteResponse (Result Error ( ItemId, Session ))
    | SliderMsg Slider.Msg
    | CloseDialog


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update session msg model =
    case msg of
        SliderMsg subMsg ->
            let
                ( newSlider, newCmd ) =
                    Slider.update subMsg model.slider
            in
            ( { model | slider = newSlider }, session, Cmd.map SliderMsg newCmd )

        -- EditItem itemId ->
        --     ( model, Route.newUrl (Route.EditItem itemId) )
        Delete ->
            ( { model | dialog = Just deleteDialog }, session, Cmd.none )

        DeleteExecute ->
            ( { model | dialog = Just <| Dialog.loadingDialog "削除処理中..." }
            , session
            , Item.delete session model.item.id |> Task.attempt DeleteResponse
            )

        DeleteResponse (Ok ( itemId, updatedSession )) ->
            ( { model | dialog = Nothing }
            , updatedSession
            , Cmd.none
              --            , Route.replaceUrl session.key (Route.Home Nothing)
            )

        DeleteResponse (Err err) ->
            ( { model | dialog = Nothing }, session, Cmd.none )

        PrepareDownload ->
            ( { model | dialog = Just <| downloadDialog Nothing }
            , session
            , Request.Download.download session [ model.item.id ]
                |> Task.attempt FileReady
            )

        FileReady (Ok ( url, updatedSession )) ->
            ( { model | dialog = Just (downloadDialog (Just url)) }, updatedSession, Cmd.none )

        FileReady (Err error) ->
            ( { model | dialog = Just (Dialog.closableDialog "ダウンロードの準備中にエラーが発生しました" CloseDialog) }, session, Cmd.none )

        FileDownloaded ->
            ( { model | dialog = Nothing }, session, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, session, Cmd.none )
