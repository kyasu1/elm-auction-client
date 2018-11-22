module Data.User exposing (User, UserId(..), baseDecoder, compareUserId, decoder, decoderWithToken, decoderWithToken2, dropToken, encode, encodeUserId, listQuery, loginQuery, refreshQuery, registerQuery, reloginQuery, stringToUserId, toUser, updateAccessToken, userFragment, userIdDecoder, userIdParser, userIdToHtml, userIdToString)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Url.Parser as Url


type alias User a =
    { id : UserId
    , name : String
    , email : String
    , token : a
    }


toUser : String -> String -> String -> User ()
toUser id name email =
    { id = UserId id
    , name = name
    , email = email
    , token = ()
    }


dropToken : User a -> User ()
dropToken user =
    User user.id user.name user.email ()


updateAccessToken : String -> User AuthToken -> User AuthToken
updateAccessToken accessToken user =
    { user | token = AuthToken.updateAccessToken user.token accessToken }



-- SERIALIZATION --
--
-- decoder : Decoder User
-- decoder =
--     Decode.oneOf
--         [ userDecoder
--         , Decode.field "errors" (Decode.fail "ログインに失敗しました")
--         ]


decoder : Decoder (User ())
decoder =
    baseDecoder
        |> hardcoded ()


decoderWithToken : Decoder (User AuthToken)
decoderWithToken =
    AuthToken.decoder
        |> Decode.andThen
            (\token ->
                Decode.field "user"
                    (Decode.succeed User
                        |> required "id" userIdDecoder
                        |> required "name" Decode.string
                        |> required "email" Decode.string
                        |> hardcoded token
                    )
            )


decoderWithToken2 : String -> Decoder (User AuthToken)
decoderWithToken2 keyName =
    Decode.field keyName AuthToken.decoder
        |> Decode.andThen
            (\token ->
                Decode.at [ keyName, "user" ]
                    (Decode.succeed User
                        |> required "id" userIdDecoder
                        |> required "name" Decode.string
                        |> required "email" Decode.string
                        |> hardcoded token
                    )
            )


baseDecoder : Decoder (a -> User a)
baseDecoder =
    Decode.succeed User
        |> required "id" userIdDecoder
        |> required "name" Decode.string
        |> required "email" Decode.string



-- encode : User AuthToken -> Value
-- encode user =
--     Encode.object
--         [ ( "id", encodeUserId user.id )
--         , ( "token", AuthToken.encode user.token )
--         , ( "name", Encode.string user.name )
--         , ( "email", Encode.string user.email )
--         ]
-- encode : User AuthToken -> Value
-- encode user =
--     Encode.object
--         [ ( "id", encodeUserId user.id )
--         , ( "name", Encode.string user.name )
--         , ( "email", Encode.string user.email )
--         , ( "refreshToken", AuthToken.encode user.token )
--         ]


encode : User AuthToken -> Value
encode user =
    Encode.object
        [ ( "session"
          , Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "id", encodeUserId user.id )
                        , ( "name", Encode.string user.name )
                        , ( "email", Encode.string user.email )
                        ]
                  )
                , ( "refreshToken", AuthToken.encode user.token )
                ]
          )
        ]



-- IDENTIFIERS --


type UserId
    = UserId String


userIdToString : UserId -> String
userIdToString (UserId userId) =
    userId


stringToUserId : String -> UserId
stringToUserId id =
    UserId id


userIdParser : Url.Parser (UserId -> a) a
userIdParser =
    Url.string |> Url.map stringToUserId


userIdDecoder : Decoder UserId
userIdDecoder =
    Decode.map UserId Decode.string


encodeUserId : UserId -> Value
encodeUserId (UserId userId) =
    Encode.string userId


userIdToHtml : UserId -> Html msg
userIdToHtml (UserId userId) =
    Html.text userId


compareUserId : UserId -> UserId -> Bool
compareUserId (UserId a) (UserId b) =
    a == b



-- GRAPH QL QUERY --


userFragment : String
userFragment =
    """
    fragment userFragment on User {
      id
      name
      email
    }
    """


loginQuery : String
loginQuery =
    userFragment ++ """
    mutation login($email: String!, $password: String!){
      login(input: {email: $email, password: $password}) {
        user {
          ...userFragment
        }
        refreshToken
        accessToken
      }
    }
    """


reloginQuery : String
reloginQuery =
    userFragment ++ """
        mutation Relogin($refreshToken: String!) {
          relogin(input: {refreshToken: $refreshToken}) {
            user {
              ...userFragment
            }
            refreshToken
            accessToken
            expiresIn
          }
        }
    """


refreshQuery : String
refreshQuery =
    """
        mutation Refresh($refreshToken: String!) {
          refresh(input: {refreshToken: $refreshToken}) {
            accessToken
          }
        }
    """


registerQuery : String
registerQuery =
    userFragment ++ """
    mutation regist($user: UserInput!) {
      regist(input: {user: $user}) {
        user {
          ...userFragment
        }
        refreshToken
        accessToken
      }
    }
    """


listQuery : String
listQuery =
    userFragment
        ++ """
    query Users {
      users {
        ...userFragment
      }
    }
    """
