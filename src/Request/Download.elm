module Request.Download exposing (download)

import Data.Item as Item
import Data.Session exposing (Session)
import Json.Decode as Decode
import Json.Encode as Encode
import Request.GraphQL.Error exposing (Error)
import Request.GraphQL.Query exposing (send)
import Task exposing (Task)


download : Session -> List Item.ItemId -> Task Error ( String, Session )
download session selected =
    let
        params =
            Encode.object
                [ ( "list_of_id"
                  , Encode.list Item.encodeItemId selected
                  )
                ]
    in
    send
        { decoder = Decode.field "filename" Decode.string
        , session = session
        , query = Encode.string Item.downloadQuery
        , field = "download"
        , variables = params
        }
