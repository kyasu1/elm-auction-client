module Data.AuthToken exposing
    ( AccessToken
    , AuthToken(..)
    , Reason(..)
    , RefreshToken
    , decoder
    , encode
    , getAccessToken
    , getRefreshToken
    , updateAccessToken
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias RefreshToken =
    String


type alias AccessToken =
    String


type AuthToken
    = NoToken Reason
    | Authenticated RefreshToken
    | Authorized RefreshToken AccessToken


type Reason
    = NoChallenge
    | InvalidRefreshToken
    | InvalidCredential


encode : AuthToken -> Value
encode token =
    case token of
        NoToken _ ->
            Encode.null

        Authenticated refreshToken ->
            Encode.string refreshToken

        Authorized refreshToken _ ->
            Encode.string refreshToken


decoder : Decoder AuthToken
decoder =
    Decode.oneOf
        [ Decode.map2 Authorized
            (Decode.field "refreshToken" Decode.string)
            (Decode.field "accessToken" Decode.string)
        , Decode.map Authenticated (Decode.field "refreshToken" Decode.string)
        , Decode.succeed (NoToken NoChallenge)
        ]


getAccessToken : AuthToken -> Maybe String
getAccessToken token =
    case token of
        Authorized _ accessToken ->
            Just accessToken

        _ ->
            Nothing


getRefreshToken : AuthToken -> Maybe String
getRefreshToken token =
    case token of
        Authenticated refreshToken ->
            Just refreshToken

        Authorized refreshToken _ ->
            Just refreshToken

        _ ->
            Nothing


updateAccessToken : AuthToken -> AccessToken -> AuthToken
updateAccessToken token accessToken =
    case token of
        NoToken reason ->
            token

        Authenticated refreshToken ->
            Authorized refreshToken accessToken

        Authorized refreshToken _ ->
            Authorized refreshToken accessToken



--withToken : Maybe AuthToken -> List Http.Header
--withToken maybeToken =
--    case maybeToken of
--        Just (AuthToken token) ->
--            [ Http.header "Authorization" ("Bearer " ++ token) ]
--
--        Nothing ->
--            []
--
--
--withAuthorization : Maybe AuthToken -> RequestBuilder a -> RequestBuilder a
--withAuthorization maybeToken builder =
--    case maybeToken of
--        Just (AuthToken token) ->
--            builder
--                |> withHeader "Authorization" ("Bearer " ++ token)
--
--        Nothing ->
--            builder
