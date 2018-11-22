module Request.Error exposing
    ( Error(..)
    , RequestError
    , convertHttpError
    , errorsDecoder
    , httpErrorToString
    )

import Http
import Json.Decode as Decode exposing (Decoder)


type Error
    = GraphQLError (List RequestError)
    | HttpError Http.Error


{-| A location in a GraphQL request document.
-}
type alias Location =
    { line : Int
    , column : Int
    }


locationDecoder : Decoder Location
locationDecoder =
    Decode.map2 Location
        (Decode.field "line" Decode.int)
        (Decode.field "column" Decode.int)


type alias RequestError =
    { message : String
    , locations : List Location
    }


errorsDecoder : Decoder (List RequestError)
errorsDecoder =
    Decode.list
        (Decode.map2 RequestError
            (Decode.field "message" Decode.string)
            (Decode.field "locations" (Decode.list locationDecoder))
        )


errorsResponseDecoder : Decoder (List RequestError)
errorsResponseDecoder =
    Decode.field "errors" errorsDecoder


convertHttpError : (Http.Error -> err) -> (List RequestError -> err) -> Http.Error -> err
convertHttpError wrapHttpError wrapGraphQLError httpError =
    let
        handleErrorWithResponseBody responseBody =
            responseBody
                |> Decode.decodeString errorsResponseDecoder
                |> Result.map wrapGraphQLError
                |> Result.withDefault (wrapHttpError httpError)
    in
    case httpError of
        Http.BadStatus { body } ->
            handleErrorWithResponseBody body

        Http.BadPayload _ { body } ->
            handleErrorWithResponseBody body

        _ ->
            wrapHttpError httpError


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Bad Http Status: " ++ toString response.status.code

        Http.BadPayload message response ->
            let
                _ =
                    Debug.log "message: " response.body
            in
            "Bad Http Payload: "
                ++ toString message
                ++ " ("
                ++ toString response.status.code
                ++ ")"
