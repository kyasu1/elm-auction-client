module Request.GraphQL.Helper exposing (post)

import Api exposing (apiUrl)
import Data.Item as Item
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Request.GraphQL.Error as Error exposing (Error(..))
import Task exposing (Task)


post :
    { r
        | token : Maybe String
        , query : Value
        , variables : Value
        , field : String
        , decoder : Decoder a
    }
    -> Task Error a
post { token, query, variables, field, decoder } =
    let
        headers =
            case token of
                Just t ->
                    [ Http.header "Authorization" ("Bearer " ++ t) ]

                Nothing ->
                    []

        params =
            Encode.object
                [ ( "query", query ), ( "variables", variables ) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = apiUrl "/graphql"
        , body = Http.jsonBody params
        , expect = Http.expectJson (responseDecoder field decoder)
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask
        |> Task.mapError HttpError
        |> Task.andThen
            (\response ->
                case response.data of
                    Just data ->
                        Task.succeed data

                    _ ->
                        Task.fail (GraphQLError response.errors)
            )


type alias GraphQLResponse a =
    { data : Maybe a
    , errors : List Error.RequestError
    }


responseDecoder : String -> Decoder a -> Decoder (GraphQLResponse a)
responseDecoder field decoder =
    Decode.map2
        GraphQLResponse
        --        (if field == "" then
        --            Decode.oneOf
        --                [ Decode.at [ "data" ] decoder |> Decode.map Just
        --                , Decode.at [ "data" ] (Decode.null Nothing)
        --                ]
        --         else
        --            Decode.oneOf
        --                [ Decode.at [ "data", field ] decoder |> Decode.map Just
        --                , Decode.at [ "data", field ] (Decode.null Nothing)
        --                ]
        --        )
        (Decode.oneOf
            [ Decode.at [ "data" ] decoder |> Decode.map Just
            , Decode.at [ "data" ] (Decode.null Nothing)
            , Decode.at [ "data", field ] decoder |> Decode.map Just
            , Decode.at [ "data", field ] (Decode.null Nothing)
            ]
        )
        (Decode.oneOf
            [ Decode.field "errors" Error.errorsDecoder
            , Decode.succeed []
            ]
        )
