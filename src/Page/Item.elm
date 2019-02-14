module Page.Item exposing (Model, Msg, initialTask, subscriptions, update, view)

{-| Viewing an individual item by registered users
-}

import Api exposing (apiUrl)
import Data.Condition as Condition
import Data.Item as Item exposing (Detail, Item, ItemId)
import Data.Masters as Masters exposing (Masters)
import Data.Selling as Selling
import Data.Session as Session exposing (Session)
import Data.Shipping as Shipping
import File.Download as Download
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
      , slider = Slider.initialModel "1200px"
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
    section [] <|
        [ article [ class "mw5-ns bl-ns br-ns pv3-ns ph4-ns  b--light-gray w-90 mw8-ns" ]
            [ itemView session model model.item
            ]
        , Dialog.view model.dialog
        ]


itemView : Session -> Model -> Item Detail -> Html Msg
itemView session model item =
    div [ class "" ]
        [ h4 [ class "text-sm italic text-blue py-2" ] [ text item.details.categoryPath ]
        , h3 [ class "text-base text-black py-2" ] [ text item.title ]
        , div [ class "flex flex-col md:flex-row p-2" ]
            [ imageView model item
            , detailsView model
            ]
        , div [ class "p-2 flex flex-row items-center " ]
            [ Html.node "auction-api-copy-text"
                [ Html.Attributes.attribute "data-copy-text" item.details.description
                , class "flex flow-row"
                ]
                [ div [] [ text "商品詳細" ]
                , button [ class "text-grey-darkest hover:text-blue px-2" ]
                    [ i [ class "fas fa-clipboard" ] []
                    ]
                , Html.node "auction-api-copy-text-note" [] []
                ]
            ]
        , div [ class "border rounded text-sm p-4 leading-loose" ]
            [ Markdown.toHtml [] item.details.description ]
        , buttonsView session model item
        ]


sortImages : Item Detail -> List String
sortImages item =
    item.details.images
        |> List.sortBy .order
        |> List.map .data


imageView : Model -> Item Detail -> Html Msg
imageView { slider } item =
    div [ class "w-full md:w-2/5 lg:w-1/2 flex-no-shrink" ]
        [ Slider.view (sortImages item) slider |> Html.map SliderMsg
        ]


detailsView : Model -> Html Msg
detailsView { item, masters } =
    let
        styleTh =
            "font-bold text-black border-b border-grey text-right p-2"

        styleTd =
            "text-black border-b border-grey text-left p-2"

        row ( l, v ) =
            tr [ class "" ]
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
    div [ class "w-full py-1 md:py-3 px-2" ]
        [ table [ class "w-full" ]
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
                [ class Style.buttonSmallBlue

                -- , onClick (EditItem item.id)
                , Route.href (Route.EditItem item.id)
                ]
                [ i [ class "fas fa-edit mr-2" ] [], text "編集" ]

        buttonDelete =
            button
                [ class Style.buttonSmallRed
                , onClick Delete
                ]
                [ i [ class "fas fa-trash-alt mr-2" ] [], text "削除" ]

        buttonDownload =
            button
                [ class Style.buttonSmallGrey
                , onClick PrepareDownload
                ]
                [ i [ class "fas fa-download mr-2" ] []
                , span [] [ text "ダウンロード" ]
                ]

        buttonHome =
            a
                [ class Style.buttonSmallBlue
                , Route.href Route.Home
                ]
                [ i [ class "fas fa-list-ul mr-2" ] [], text "一覧へ" ]
    in
    div []
        [ div [ class "flex flex-col md:flex-row" ]
            (case session.user of
                Just user ->
                    if user.id == item.userId then
                        [ div [ class "p-2" ] [ buttonEdit ]
                        , div [ class "p-2" ] [ buttonDelete ]
                        , div [ class "p-2" ] [ buttonDownload ]
                        , div [ class "p-2" ] [ buttonHome ]
                        ]

                    else
                        [ div [ class "p-2" ] [ buttonDownload ]
                        , div [ class "p-2" ] [ buttonHome ]
                        ]

                Nothing ->
                    [ div [ class "p-2" ] [ buttonHome ]
                    ]
            )
        ]


downloadDialog : Maybe String -> Html Msg
downloadDialog maybeFileName =
    div []
        [ case maybeFileName of
            Just fileName ->
                div [ onClick (FileDownload fileName) ] [ text "ダウンロード" ]

            Nothing ->
                div [] [ text "ダウンロードの準備中" ]
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
    | FileDownload String
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

        FileReady (Ok ( fileName, updatedSession )) ->
            ( { model | dialog = Just (downloadDialog (Just fileName)) }, updatedSession, Cmd.none )

        FileReady (Err error) ->
            ( { model | dialog = Just (Dialog.closableDialog "ダウンロードの準備中にエラーが発生しました" CloseDialog) }, session, Cmd.none )

        FileDownload fileName ->
            ( { model | dialog = Nothing }, session, Download.url <| apiUrl <| "/download?filename=" ++ fileName )

        CloseDialog ->
            ( { model | dialog = Nothing }, session, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SliderMsg (Slider.subscriptions (sortImages model.item) model.slider)
