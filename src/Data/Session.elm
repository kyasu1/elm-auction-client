module Data.Session exposing (Session, attempt, updateAccessToken)

import Browser.Navigation exposing (Key)
import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.User as User exposing (User)


type alias Session =
    { user : Maybe (User AuthToken)
    , key : Key
    }


updateAccessToken : Session -> String -> Session
updateAccessToken session accessToken =
    { session
        | user =
            Maybe.map
                (User.updateAccessToken accessToken)
                session.user
    }


attempt : String -> (AuthToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
    case Maybe.map .token session.user of
        Nothing ->
            ( [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ], Cmd.none )

        Just token ->
            ( [], toCmd token )
