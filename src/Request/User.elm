module Request.User exposing (list, login, refresh, register, relogin, storeSession)

import Data.AuthToken as AuthToken exposing (AuthToken, RefreshToken)
import Data.Session exposing (Session)
import Data.User as User exposing (User)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ports
import Request.GraphQL.Error exposing (Error)
import Request.GraphQL.Helper as Helper
import Task exposing (Task)


storeSession : User AuthToken -> Cmd msg
storeSession user =
    User.encode user
        |> Encode.encode 0
        -- |> Just
        |> Ports.StoreSession
        |> Ports.sendInfoOutside


login : { email : String, password : String } -> Task Error (User AuthToken)
login { email, password } =
    let
        variables =
            Encode.object [ ( "email", Encode.string email ), ( "password", Encode.string password ) ]
    in
    Helper.post
        { token = Nothing
        , query = Encode.string User.loginQuery
        , variables = variables
        , field = "login"
        , decoder = User.decoderWithToken
        }


relogin : RefreshToken -> Task Error (User AuthToken)
relogin refreshToken =
    Helper.post
        { token = Just refreshToken
        , query = Encode.string User.reloginQuery
        , variables = Encode.object [ ( "refreshToken", Encode.string refreshToken ) ]
        , field = "relogin"
        , decoder = User.decoderWithToken
        }


refresh : RefreshToken -> Task Error (Maybe String)
refresh refreshToken =
    Helper.post
        { token = Just refreshToken
        , query = Encode.string User.refreshQuery
        , variables = Encode.object [ ( "refreshToken", Encode.string refreshToken ) ]
        , field = "refresh"
        , decoder = Decode.field "accessToken" (Decode.nullable Decode.string)
        }


register : Session -> { r | name : String, email : String, password : String } -> Task Error (User AuthToken)
register session { name, email, password } =
    let
        variables =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "name", Encode.string name )
                        , ( "email", Encode.string email )
                        , ( "password", Encode.string password )
                        ]
                  )
                ]
    in
    Helper.post
        { token = Nothing
        , query = Encode.string User.registerQuery
        , variables = variables
        , field = "regist"
        , decoder = User.decoderWithToken
        }


list : Task Error (List (User ()))
list =
    let
        decoder : Decoder (List (User ()))
        decoder =
            Decode.list User.decoder
    in
    Helper.post
        { token = Nothing
        , query = Encode.string User.listQuery
        , variables = Encode.null
        , field = "users"
        , decoder = decoder
        }
