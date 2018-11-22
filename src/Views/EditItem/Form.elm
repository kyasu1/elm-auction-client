module Views.EditItem.Form exposing
    ( FormData
    , ItemForm
    , ItemFormError(..)
    , errorToString
    , initialize
    , toItem
    , validation
    )

import Bitwise
import Data.Condition as Condition exposing (Condition)
import Data.Item as Item exposing (Detail, Image, Item, ItemId)
import Data.Selling as Selling exposing (Selling)
import Data.Shipping as Shipping exposing (Payer)
import Data.User as User exposing (User)
import Date
import Form
import Form.Error
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (..)
import Regex
import Time
import Util exposing (restLength, restSJISLength)


type alias FormData =
    { id : String
    , title : String
    , itemCode : String
    , categoryId : Int
    , categoryPath : String
    , condition : Condition
    , conditionDetail : String
    , description : String
    , keywords : String
    , paymentMethod : String
    , shipping : Payer
    , images : List Item.Image
    , selling : Selling
    }


type ItemFormError
    = InvalidSJISLength Float
    | PriceComparison
    | PriceZero
    | InvalidSellingType
    | InvalidCondition


errorToString : ItemFormError -> String
errorToString error =
    case error of
        InvalidSJISLength rest ->
            String.fromFloat rest ++ "文字オーバーしています"

        PriceComparison ->
            "即決価格が開始価格小さいです"

        PriceZero ->
            "0より大きな数字にしてください"

        InvalidSellingType ->
            "不正な販売方法が選択されました"

        InvalidCondition ->
            "状態の選択に誤りがあります"


type alias ItemForm =
    Form.Form ItemFormError FormData



-- VALIDATION --


validation : Validation ItemFormError FormData
validation =
    succeed FormData
        |> andMap (field "id" string |> defaultValue "")
        |> andMap (field "title" (string |> andThen (validateSJISLength 65)))
        |> andMap (field "itemCode" (string |> defaultValue "" |> andThen (maxLength 20)))
        |> andMap (field "categoryId" (int |> andThen (minInt 1)))
        |> andMap (field "categoryPath" (string |> andThen nonEmpty))
        |> andMap (field "condition" (string |> andThen validateCondition))
        |> andMap (field "conditionDetail" (string |> defaultValue "" |> andThen (validateSJISLength 15)))
        |> andMap (field "description" (string |> andThen (validateSJISLength 25000)))
        |> andMap (field "keywords" (string |> defaultValue "" |> andThen (validateSJISLength 20)))
        |> andMap (field "paymentMethod" string)
        |> andMap (field "shipping" validatePayer)
        |> andMap (field "images" (list validateImage))
        |> andMap (field "selling" validateSelling)


validateCondition : String -> Validation ItemFormError Condition
validateCondition condition field =
    case condition of
        "1" ->
            succeed Condition.New field

        "4" ->
            succeed Condition.RankS field

        "5" ->
            succeed Condition.RankA field

        "6" ->
            succeed Condition.RankB field

        "7" ->
            succeed Condition.RankC field

        "8" ->
            succeed Condition.RankD field

        _ ->
            fail (customError InvalidCondition) field


validateSelling : Validation ItemFormError Selling
validateSelling =
    andThen
        (\method ->
            case method of
                "オークション形式" ->
                    map2 Selling.Auction
                        (field "priceOpen" (int |> andThen (minInt 1) |> withCustomError PriceZero))
                        (field "priceOpen" int |> andThen checkPrices)

                "即決形式" ->
                    map2 Selling.Fixed
                        (field "priceBuyout" (int |> andThen (minInt 1) |> withCustomError PriceZero))
                        (field "allowDiscount" bool)

                _ ->
                    fail (customError InvalidSellingType)
        )
        (field "method" string)


validatePayer : Validation ItemFormError Payer
validatePayer =
    andThen
        (\payer ->
            case payer of
                "出品者" ->
                    succeed Shipping.Seller

                "落札者" ->
                    andThen
                        (\method ->
                            if method == "宅急便" then
                                andThen
                                    (\size ->
                                        succeed (Shipping.Buyer <| Shipping.Takkyubin (Shipping.toSize size))
                                    )
                                    (field "size" string)

                            else
                                succeed (Shipping.Buyer <| Shipping.toMethod method)
                        )
                        (field "method" string)

                _ ->
                    succeed Shipping.Seller
        )
        (field "payer" string)


validateSJISLength : Float -> String -> Validation ItemFormError String
validateSJISLength maxLength s field =
    let
        rest =
            restSJISLength maxLength s
    in
    if rest < 0 then
        Err (customError (InvalidSJISLength <| negate rest))

    else
        Ok s


validateImage : Validation ItemFormError Item.Image
validateImage =
    map4 Item.Image
        (field "id" (string |> defaultValue ""))
        (field "order" int)
        (field "data" string |> defaultValue "")
        (field "auctionId" (string |> defaultValue ""))


checkPrices : Int -> Validation ItemFormError Int
checkPrices priceOpen =
    field "priceBuyout"
        (int
            |> andThen
                (\priceBuyout ->
                    if priceBuyout == 0 || priceBuyout >= priceOpen then
                        succeed priceBuyout

                    else
                        fail (customError PriceComparison)
                )
        )


initialize : Item Detail -> List ( String, Field )
initialize item =
    [ ( "id", Field.string (Item.itemIdToString item.id) )
    , ( "title", Field.string item.title )
    , ( "itemCode", Field.string item.details.itemCode )
    , ( "selling", initSelling item )
    , ( "categoryId", Field.string (String.fromInt item.details.categoryId) )
    , ( "categoryPath", Field.string item.details.categoryPath )
    , ( "condition", Field.string <| Condition.toString item.details.condition )
    , ( "conditionDetail", Field.string item.details.conditionDetail )
    , ( "description", Field.string item.details.description )
    , ( "keywords", Field.string item.details.keywords )
    , ( "paymentMethod", Field.string item.details.paymentMethod )
    , ( "images", Field.list (List.map imageField item.details.images) )
    , ( "shipping", initPayer item )
    ]


initSelling : Item Detail -> Field
initSelling item =
    case item.selling of
        Selling.Auction priceOpen priceBuyout ->
            Field.group
                [ ( "method", Field.string "オークション形式" )
                , ( "priceOpen", Field.string (String.fromInt priceOpen) )
                , ( "priceBuyout", Field.string (String.fromInt priceBuyout) )
                ]

        Selling.Fixed priceBuyout allowDiscount ->
            Field.group
                [ ( "method", Field.string "即決形式" )
                , ( "priceBuyout", Field.string (String.fromInt priceBuyout) )
                , ( "allowDiscount", Field.bool allowDiscount )
                ]


initPayer : Item Detail -> Field
initPayer item =
    case item.details.shipping of
        Shipping.Seller ->
            Field.group
                [ ( "payer", Field.string "出品者" )
                , ( "method", Field.string "" )
                , ( "size", Field.string "" )
                ]

        Shipping.Buyer method ->
            case method of
                Shipping.Takkyubin size ->
                    Field.group
                        [ ( "payer", Field.string "落札者" )
                        , ( "method", Field.string "宅急便" )
                        , ( "size", Field.string <| Shipping.fromSize size )
                        ]

                _ ->
                    Field.group
                        [ ( "payer", Field.string "落札者" )
                        , ( "method", Field.string <| Shipping.fromMethod method )
                        , ( "size", Field.string "" )
                        ]


imageField : Image -> Field.Field
imageField image =
    Field.group
        [ ( "id", Field.string image.id )
        , ( "order", Field.string <| String.fromInt image.order )
        , ( "data", Field.string <| image.data )
        , ( "itemId", Field.string image.itemId )
        ]



--


toItem : FormData -> User a -> List Image -> Item Detail
toItem formData user images =
    let
        details =
            { itemCode = formData.itemCode
            , categoryId = formData.categoryId
            , categoryPath = formData.categoryPath
            , condition = formData.condition
            , conditionDetail = formData.conditionDetail
            , description = formData.description
            , keywords = formData.keywords
            , paymentMethod = formData.paymentMethod
            , images = images
            , shipping = formData.shipping
            }

        item =
            { id = Item.toItemId formData.id
            , title = formData.title
            , selling = formData.selling
            , thumb = ""
            , userId = user.id
            , userName = user.name
            , insertedAt = Date.fromCalendarDate 2010 Time.Jul 7
            , updatedAt = Date.fromCalendarDate 2010 Time.Jul 7
            , details = details
            }
    in
    item
