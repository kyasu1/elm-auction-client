module Request.GraphQL.Query exposing (send)

import Data.AuthToken as AuthToken exposing (AuthToken(..))
import Data.Session exposing (Session)
import Data.User as User exposing (User)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Request.GraphQL.Error as Error exposing (Error(..))
import Request.GraphQL.Helper as Helper
import Request.User as User
import Task exposing (Task)



{--This tries to send a GraphQL query with an access token.
If the token is not valid, this tries to get a new access token by
sending the provided refresh token. Then try to get a resource again.
--}


send :
    { r
        | session : Session
        , query : Value
        , variables : Value
        , field : String
        , decoder : Decoder a
    }
    -> Task Error ( a, Session )
send { session, query, variables, field, decoder } =
    let
        request maybeToken =
            Helper.post
                { token = maybeToken
                , query = query
                , variables = variables
                , field = field
                , decoder = decoder
                }

        refresh : String -> User AuthToken -> Task Error ( a, Session )
        refresh refreshToken user =
            User.refresh refreshToken
                |> Task.andThen
                    (\maybeToken ->
                        case maybeToken of
                            Just token ->
                                let
                                    updatedSession =
                                        { session
                                            | user =
                                                Just (User.updateAccessToken token user)
                                        }
                                in
                                ( request (Just token), updatedSession ) |> toTuple

                            Nothing ->
                                ( Task.fail NotAuthorized, session ) |> toTuple
                    )
    in
    case session.user of
        Just user ->
            case user.token of
                Authorized refreshToken accessToken ->
                    ( request (Just accessToken), session )
                        |> toTuple
                        |> Task.onError
                            (\error ->
                                case error of
                                    GraphQLError e ->
                                        refresh refreshToken user

                                    _ ->
                                        ( Task.fail error, session ) |> toTuple
                            )

                Authenticated refreshToken ->
                    refresh refreshToken user

                NoToken _ ->
                    ( request Nothing, session ) |> toTuple

        Nothing ->
            ( request Nothing, session ) |> toTuple


toTuple : ( Task Error a, Session ) -> Task Error ( a, Session )
toTuple ( task, session ) =
    task |> Task.map (\t -> ( t, session ))
