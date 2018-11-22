module Request.GraphQL.Relay exposing (PageInfo, Relay, decoder, emptyPageInfo, pageInfoDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)


type alias PageInfo =
    { startCursor : String
    , hasPreviousPage : Bool
    , hasNextPage : Bool
    , endCursor : String
    }


emptyPageInfo : PageInfo
emptyPageInfo =
    { startCursor = ""
    , hasPreviousPage = False
    , hasNextPage = False
    , endCursor = ""
    }


pageInfoDecoder : Decoder PageInfo
pageInfoDecoder =
    Decode.succeed PageInfo
        |> optional "startCursor" Decode.string ""
        |> required "hasPreviousPage" Decode.bool
        |> required "hasNextPage" Decode.bool
        |> optional "endCursor" Decode.string ""


type alias Relay a =
    { pageInfo : PageInfo
    , edges : List a
    }


decoder : Decoder a -> Decoder (Relay a)
decoder edgesDecoder =
    Decode.succeed Relay
        |> required "pageInfo" pageInfoDecoder
        |> required "edges" (Decode.list edgesDecoder)
