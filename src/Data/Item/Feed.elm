module Data.Item.Feed exposing
    ( Feed
    , append
    , copy
    , deletable
    , delete
    , deleteMultiple
    , deselectAll
    , downlodable
    , getSelected
    , list
    , more
    , prependItem
    , removeItem
    , removeMultiple
    , selectAll
    , setSelected
    )

import Data.Item as Item exposing (Item, ItemId)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (UserId, encodeUserId)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Request.GraphQL.Error exposing (Error)
import Request.GraphQL.Query as Query
import Request.GraphQL.Relay as Helpers exposing (PageInfo)
import Request.Item as Item exposing (ListConfig, defaultListConfig, list)
import Task exposing (Task)


type alias Feed =
    { pageInfo : PageInfo
    , userId : Maybe UserId
    , query : Maybe String
    , items : List { item : Item (), selected : Bool }
    }


init : ListConfig -> ( Helpers.Relay (Item ()), Session ) -> ( Feed, Session )
init config ( relay, session ) =
    ( { pageInfo = relay.pageInfo
      , userId = config.userId
      , query = config.query
      , items = List.map (\item -> { item = item, selected = False }) relay.edges
      }
    , session
    )


setSelected : ItemId -> Feed -> Feed
setSelected itemId feed =
    { feed
        | items =
            List.map
                (\e ->
                    if Item.equalId e.item.id itemId then
                        { e | selected = not e.selected }

                    else
                        e
                )
                feed.items
    }


getSelected : Feed -> List (Item ())
getSelected { items } =
    items
        |> List.filter (\item -> item.selected == True)
        |> List.map .item



-- PRPEND AN ITEM TO EXISTING FEED


prependItem : Item () -> Feed -> Feed
prependItem item feed =
    { feed | items = { item = item, selected = False } :: feed.items }



-- REMOVE AN ITEM FROM EXISTING FEED


removeItem : ItemId -> Feed -> Feed
removeItem itemId feed =
    { feed | items = List.filter (\e -> not (Item.equalId e.item.id itemId)) feed.items }



-- REMOVE AN ITEM FROM EXISTING FEED


removeMultiple : List (Item ()) -> Feed -> Feed
removeMultiple listOfItem feed =
    { feed | items = List.filter (\e -> not (List.member e.item listOfItem)) feed.items }



-- APPEND FEED TO EXISTING FEED


append : Feed -> Feed -> Feed
append current next =
    { next
        | items = List.concat [ current.items, next.items ]
    }


selectAll : Feed -> Feed
selectAll feed =
    { feed | items = List.map (\e -> { e | selected = True }) feed.items }


deselectAll : Feed -> Feed
deselectAll feed =
    { feed | items = List.map (\e -> { e | selected = False }) feed.items }


downlodable : Feed -> Bool
downlodable feed =
    List.any (\e -> e.selected == True) feed.items


deletable : Session -> Feed -> Bool
deletable session feed =
    case session.user of
        Just user ->
            -- feed.items
            --     |> List.filter (\e -> e.selected == True)
            --     |> List.all (\e -> e.item.userId /= user.id)
            --     |> not
            let
                selected =
                    feed.items |> List.filter (\e -> e.selected == True)
            in
            if List.length selected > 0 then
                List.all (\e -> e.item.userId == user.id) selected

            else
                False

        Nothing ->
            False



-- LIST --


list : ListConfig -> Session -> Task Error ( Feed, Session )
list config session =
    Item.list config session |> Task.map (init config)


more : Session -> Feed -> Task Error ( Feed, Session )
more session feed =
    let
        config =
            { defaultListConfig | query = feed.query, userId = feed.userId, cursor = Just feed.pageInfo.endCursor }
    in
    Item.list config session |> Task.map (init config)



-- DUPLICATE --


copy : ItemId -> Session -> Task Error ( Item (), Session )
copy itemId session =
    let
        variables =
            Encode.object
                [ ( "id", Item.encodeItemId itemId )
                ]
    in
    Query.send
        { session = session
        , query = Encode.string copyQuery
        , variables = variables
        , field = "copyItem"
        , decoder = Item.decoder
        }



-- DELETE --


delete : ItemId -> Session -> Task Error ( Item (), Session )
delete itemId session =
    let
        variables =
            Encode.object
                [ ( "id", Item.encodeItemId itemId )
                ]
    in
    Query.send
        { session = session
        , query = Encode.string deleteQuery
        , variables = variables
        , field = "deleteItem"
        , decoder = Item.decoder
        }



-- DELETE --


deleteMultiple : List (Item ()) -> Session -> Task Error ( List ItemId, Session )
deleteMultiple listOfItem session =
    let
        variables =
            Encode.object
                [ ( "list_of_id", Encode.list Item.encodeItemId (List.map .id listOfItem) )
                ]
    in
    Query.send
        { session = session
        , query = Encode.string deleteListQuery
        , variables = variables
        , field = "deleteItems"
        , decoder = Decode.list (Decode.field "id" Item.itemIdDecoder)
        }



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


usersFragment : String
usersFragment =
    """
      id
      name
      email
    """



--listQuery : String
--listQuery =
--    itemFragment
--        ++ """
--  query auctions($first: Int!, $after: String, $query: String, $userId: String) {
--    auctions(first: $first, after: $after, query: $query, userId: $userId ) {
--      totalCount
--      pageInfo {
--        hasNextPage
--        hasPreviousPage
--        startCursor
--        endCursor
--      }
--      edges {
--        node{
--        ...itemFragment
--        }
--      }
--    }
--  }
--"""


copyQuery : String
copyQuery =
    itemFragment
        ++ """
    mutation CopyItem($id: ID!) {
      copyItem(id: $id) {
        ...itemFragment
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


deleteListQuery : String
deleteListQuery =
    """ mutation DeleteItems($list_of_id: [ID!]) {
      deleteItems(list_of_id: $list_of_id) {
        id
      }
    }
  """
