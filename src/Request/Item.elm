module Request.Item exposing (ListConfig, defaultListConfig, delete, get, list, update)

import Data.Item as Item exposing (Detail, Item, ItemId)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (UserId)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Request.GraphQL.Error exposing (Error)
import Request.GraphQL.Query as Query
import Request.GraphQL.Relay as Relay exposing (Relay)
import Task exposing (Task)



-- SINGLE --


get : Session -> ItemId -> Task Error ( Item Detail, Session )
get session id =
    let
        variables =
            Encode.object
                [ ( "id", Encode.string (Item.itemIdToString id) ) ]
    in
    Query.send
        { session = session
        , query = Encode.string Item.getQuery
        , variables = variables
        , field = "item"
        , decoder = Item.decoderWithDetail
        }



-- LIST --


type alias ListConfig =
    { query : Maybe String
    , userId : Maybe UserId
    , perPage : Int
    , cursor : Maybe String
    }


defaultListConfig : ListConfig
defaultListConfig =
    { query = Nothing
    , userId = Nothing
    , perPage = 20
    , cursor = Nothing
    }


list : ListConfig -> Session -> Task Error ( Relay (Item ()), Session )
list config session =
    let
        query =
            config.query
                |> Maybe.map (\q -> [ ( "query", Encode.string q ) ])
                |> Maybe.withDefault []

        after =
            config.cursor
                |> Maybe.map (\c -> [ ( "after", Encode.string c ) ])
                |> Maybe.withDefault []

        userId =
            config.userId
                |> Maybe.map (\u -> [ ( "userId", User.encodeUserId u ) ])
                |> Maybe.withDefault []

        variables =
            Encode.object
                (List.concat
                    [ query
                    , after
                    , userId
                    , [ ( "first", Encode.int config.perPage )
                      ]
                    ]
                )

        decoder =
            Relay.decoder (Decode.field "node" Item.decoder)
    in
    Query.send
        { session = session
        , query = Encode.string Item.listQuery
        , variables = variables
        , field = "items"
        , decoder = decoder
        }



-- UPDATE --


update : Session -> Item Detail -> Task Error ( Item Detail, Session )
update session item =
    let
        variables =
            Encode.object
                [ ( "item", Item.encode item )
                ]

        decoder =
            Decode.field "item" Item.decoderWithDetail
    in
    if Item.hasValue item.id then
        Query.send
            { session = session
            , query = Encode.string Item.updateQuery
            , variables = variables
            , field = "updateItem"
            , decoder = decoder
            }

    else
        Query.send
            { session = session
            , query = Encode.string Item.createQuery
            , variables = variables
            , field = "createItem"
            , decoder = decoder
            }



-- DELETE --


delete : Session -> ItemId -> Task Error ( ItemId, Session )
delete session id =
    let
        variables =
            Encode.object
                [ ( "id", Encode.string (Item.itemIdToString id) ) ]

        decoder =
            Decode.at [ "id" ] Item.itemIdDecoder
    in
    Query.send
        { session = session
        , query = Encode.string Item.deleteQuery
        , variables = variables
        , field = "deleteItem"
        , decoder = decoder
        }
