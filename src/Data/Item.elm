module Data.Item exposing
    ( Detail
    , Image
    , Item
    , ItemId
    , createQuery
    , decoder
    , decoderWithDetail
    , deleteQuery
    , downloadQuery
    , encode
    , encodeItemId
    , equalId
    , getQuery
    , hasValue
    , initialItem
    , itemIdDecoder
    , itemIdParser
    , itemIdToString
    , listQuery
    , toItemId
    , updateQuery
    )

import Data.Condition as Condition exposing (Condition)
import Data.Selling as Selling exposing (Selling)
import Data.Shipping as Shipping exposing (Payer)
import Data.User as User exposing (User)
import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode exposing (Value)
import Time
import Url.Parser as Parser


type alias Item a =
    { id : ItemId
    , title : String
    , selling : Selling
    , thumb : String
    , userId : User.UserId
    , userName : String
    , insertedAt : Date
    , updatedAt : Date
    , details : a
    }


type alias Detail =
    { itemCode : String
    , categoryId : Int
    , categoryPath : String
    , condition : Condition
    , conditionDetail : String
    , description : String
    , keywords : String
    , paymentMethod : String
    , images : List Image
    , shipping : Payer
    }



-- DEFAULT --


initialItem : User a -> Item Detail
initialItem user =
    { id = toItemId ""
    , title = ""
    , selling = Selling.Fixed 0 False
    , thumb = ""
    , userId = user.id
    , userName = user.name
    , insertedAt = Date.fromCalendarDate 2010 Time.Jul 7
    , updatedAt = Date.fromCalendarDate 2010 Time.Jul 7
    , details = initialDetail
    }


initialDetail : Detail
initialDetail =
    { itemCode = ""
    , categoryId = 0
    , categoryPath = ""
    , condition = Condition.RankA
    , conditionDetail = ""
    , description = ""
    , keywords = ""
    , paymentMethod = "1"
    , images = List.map initialImage (List.range 1 10)
    , shipping = Shipping.Seller
    }



-- SERIALIZATION --


decoder : Decoder (Item ())
decoder =
    baseItemDecoder
        |> hardcoded ()


decoderWithDetail : Decoder (Item Detail)
decoderWithDetail =
    baseItemDecoder
        |> custom detailDecoder


itemIdDecoder : Decoder ItemId
itemIdDecoder =
    Decode.map ItemId Decode.string


baseItemDecoder : Decoder (a -> Item a)
baseItemDecoder =
    Decode.succeed Item
        |> required "id" (Decode.map ItemId Decode.string)
        |> required "title" Decode.string
        |> required "selling" Selling.decoder
        |> required "thumb" Decode.string
        |> custom (Decode.at [ "user", "id" ] User.userIdDecoder)
        |> custom (Decode.at [ "user", "name" ] Decode.string)
        |> required "insertedAt" dateDecoder
        |> required "updatedAt" dateDecoder


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\stringDate ->
                case Date.fromIsoString (stringDate |> String.left 10) of
                    Ok date ->
                        Decode.succeed date

                    Err err ->
                        Decode.fail err
            )


detailDecoder : Decoder Detail
detailDecoder =
    Decode.succeed Detail
        |> required "itemCode" Decode.string
        |> required "categoryId" Decode.int
        |> required "categoryPath" Decode.string
        |> required "condition" Condition.decoder
        |> required "conditionDetail" Decode.string
        |> required "description" Decode.string
        |> required "keywords" Decode.string
        |> required "paymentMethod" Decode.string
        |> required "images" (Decode.list imageDecoder)
        |> required "shipping" Shipping.decoder


encode : Item Detail -> Value
encode item =
    Encode.object
        [ ( "id", encodeItemId item.id )
        , ( "userId", User.encodeUserId item.userId )
        , ( "title", Encode.string item.title )
        , ( "selling", Selling.encoder item.selling )
        , ( "itemCode", Encode.string item.details.itemCode )
        , ( "categoryId", Encode.int item.details.categoryId )
        , ( "categoryPath", Encode.string item.details.categoryPath )
        , ( "condition", Condition.encoder item.details.condition )
        , ( "conditionDetail", Encode.string item.details.conditionDetail )
        , ( "description", Encode.string item.details.description )
        , ( "keywords", Encode.string item.details.keywords )
        , ( "paymentMethod", Encode.string item.details.paymentMethod )
        , ( "images", Encode.list encodeImage item.details.images )
        , ( "shipping", Shipping.encoder item.details.shipping )
        ]



-- IDENTIFIERS --


type ItemId
    = ItemId String


itemIdToString : ItemId -> String
itemIdToString (ItemId id) =
    id


itemIdParser : Parser.Parser (ItemId -> a) a
itemIdParser =
    Parser.string |> Parser.map toItemId


toItemId : String -> ItemId
toItemId id =
    ItemId id


hasValue : ItemId -> Bool
hasValue (ItemId id) =
    id /= ""


equalId : ItemId -> ItemId -> Bool
equalId (ItemId a) (ItemId b) =
    a == b


encodeItemId : ItemId -> Value
encodeItemId (ItemId id) =
    Encode.string id



-- IMAGE --


type alias Image =
    { id : String
    , order : Int
    , data : String
    , itemId : String
    }


initialImage : Int -> Image
initialImage index =
    { id = ""
    , order = index
    , data = ""
    , itemId = ""
    }


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "id" Decode.string
        |> required "order" Decode.int
        |> required "data" Decode.string
        |> required "item"
            (Decode.at [ "id" ] Decode.string)


encodeImage : Image -> Value
encodeImage image =
    if image.id == "" then
        Encode.object
            [ ( "order", Encode.int image.order )
            , ( "data", Encode.string image.data )
            , ( "itemId", Encode.string image.itemId )
            ]

    else
        Encode.object
            [ ( "id", Encode.string image.id )
            , ( "order", Encode.int image.order )
            , ( "data", Encode.string image.data )
            , ( "itemId", Encode.string image.itemId )
            ]



-- GRAPHQL --


itemFragment : String
itemFragment =
    """
  fragment itemFragment on Item {
      id
      title
      selling {
        method
        priceOpen
        priceBuyout
        allowDiscount
      }
      thumb
      user {
        id
        name
      }
      insertedAt
      updatedAt
  }
  """


listQuery : String
listQuery =
    itemFragment
        ++ """
  query items($first: Int!, $after: String, $query: String, $userId: String) {
    items(first: $first, after: $after, query: $query, userId: $userId ) {
      pageInfo {
        hasNextPage
        hasPreviousPage
        startCursor
        endCursor
      }
      edges {
        node{
        ...itemFragment
        }
      }
    }
  }
"""


itemDetailFragment : String
itemDetailFragment =
    """
    fragment itemDetailFragment on Item {
      id
      title
      thumb
      user {
        id
        name
      }
      insertedAt
      updatedAt

      itemCode
      categoryId
      categoryPath
      condition
      conditionDetail
      description
      keywords
      paymentMethod
      images {
        id
        order
        data
        item {
          id
        }
      }
      shipping {
        payer
        method
        size
      }
      selling {
        method
        priceOpen
        priceBuyout
        allowDiscount
      }
    }
  """


getQuery : String
getQuery =
    itemDetailFragment
        ++ """
  query GetItemById($id: ID!) {
    item(id: $id) {
      ...itemDetailFragment
    }
  }
"""


updateQuery : String
updateQuery =
    itemDetailFragment
        ++ """
mutation UpdateItem($item: ItemInput!) {
  updateItem(input: {item: $item}) {
    item {
      ...itemDetailFragment
    }
  }
}
"""


createQuery : String
createQuery =
    itemDetailFragment
        ++ """
mutation CreateItem($item: ItemInput!) {
  createItem(input: {item: $item}) {
    item {
      ...itemDetailFragment
    }
  }
}
"""


deleteQuery : String
deleteQuery =
    """ mutation DeleteItem($id: ID!) {
  deleteItem(id: $id) {
    id
  }
}
"""


downloadQuery : String
downloadQuery =
    """
  query Download($list_of_id: ID!) {
    download(list_of_id: $list_of_id) {
      filename
    }
  }
"""
