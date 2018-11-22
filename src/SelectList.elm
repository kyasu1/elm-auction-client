module SelectList exposing (ListItem, init, select, view)

import Data.User as User exposing (User, UserId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List.Selection as Selection exposing (Selection)


type ListItem
    = All
    | User (User ())
    | Spreader


init : List (User ()) -> Selection ListItem
init users =
    Selection.fromList <|
        [ All
        , Spreader
        ]
            ++ List.map (\user -> User user) users


select : String -> Selection ListItem -> Selection ListItem
select value items =
    if value == "" then
        Selection.deselect items

    else
        items
            |> Selection.selectBy
                (\item ->
                    case item of
                        User user ->
                            User.userIdToString user.id == value

                        _ ->
                            False
                )


view : (String -> msg) -> Maybe UserId -> Selection ListItem -> Html msg
view message userId list =
    Html.select
        [ class "input-reset f6 ba pv2 ph3 mr2"
        , on "change" <| Decode.map message <| Decode.at [ "target", "value" ] Decode.string
        ]
        (Selection.toList list |> List.map (options userId))


options : Maybe UserId -> ListItem -> Html msg
options userId item =
    case item of
        All ->
            option [ value "" ] [ text "全ユーザー" ]

        User a ->
            let
                s =
                    case userId of
                        Just id ->
                            User.compareUserId id a.id

                        Nothing ->
                            False
            in
            option [ value <| User.userIdToString a.id, selected s ] [ text a.name ]

        Spreader ->
            option [ Html.Attributes.disabled True ] [ text "------------" ]
