port module Ports exposing
    ( InfoForElm(..)
    ,  -- ( date
       -- , onSessionChange
       -- , setEditState
       InfoForOutside(..)

    , getInfoFromOutside
    , sendInfoOutside
    )

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.User as User exposing (User)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)



-- port date : (Value -> msg) -> Sub msg
-- port onSessionChange : (Value -> msg) -> Sub msg


type alias GenericOutsideData =
    { tag : String, data : Encode.Value }


type InfoForOutside
    = LogError String
    | StoreSession String
    | ClearSession
    | SetEditState Bool
    | ReplaceState String
    | CopyToClipBoard String


type InfoForElm
    = DateTime
    | SessionChange (Maybe (User AuthToken))


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
    case info of
        LogError err ->
            infoForOutside { tag = "LogError", data = Encode.string err }

        StoreSession session ->
            infoForOutside { tag = "StoreSession", data = Encode.string session }

        ClearSession ->
            infoForOutside { tag = "ClearSession", data = Encode.null }

        SetEditState editting ->
            infoForOutside { tag = "SetEditState", data = Encode.bool editting }

        ReplaceState state ->
            infoForOutside { tag = "ReplaceState", data = Encode.string state }

        CopyToClipBoard string ->
            infoForOutside { tag = "CopyToClipBoard", data = Encode.string string }


getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
    infoForElm
        (\outsideInfo ->
            case outsideInfo.tag of
                "SessionChange" ->
                    Decode.decodeValue (Decode.field "session" User.decoderWithToken) outsideInfo.data
                        |> Result.toMaybe
                        |> SessionChange
                        |> tagger

                _ ->
                    onError <| "Unexpected info from ouside: " ++ Debug.toString outsideInfo
        )
