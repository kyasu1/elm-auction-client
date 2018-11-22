module Request.Masters exposing (get)

import Data.Masters as Masters exposing (Masters)
import Data.Session as Session exposing (Session)
import Json.Encode as Encode
import Request.GraphQL.Error exposing (Error)
import Request.GraphQL.Query as Query
import Task exposing (Task)



-- SINGLE --


get : Session -> Task Error ( Masters, Session )
get session =
    let
        variables =
            Encode.object
                []
    in
    Query.send
        { session = session
        , query = Encode.string Masters.getQuery
        , variables = variables
        , field = ""
        , decoder = Masters.decoder
        }
