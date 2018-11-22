module Route exposing (Route(..), href, parseUrl, pathFor, pushUrl, replaceUrl)

import Browser.Navigation as Nav
import Data.Item as Item
import Data.User as User
import Html exposing (Attribute)
import Html.Attributes
import Http
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Url exposing ((</>), (<?>), Parser, oneOf, parse, s, string, top)
import Url.Parser.Query as Query



-- ROUTING --


type Route
    = Home
    | Feed User.UserId
    | Search (Maybe String)
    | Login
    | Logout
    | Register
    | Item Item.ItemId
    | EditItem Item.ItemId
    | NewItem
    | NotFound


matches : Parser (Route -> a) a
matches =
    oneOf
        [ Url.map Search (s "search" <?> Query.string "q")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Register (s "register")
        , Url.map NewItem (s "item" </> s "new")
        , Url.map EditItem (s "item" </> s "edit" </> Item.itemIdParser)
        , Url.map Item (s "item" </> Item.itemIdParser)
        , Url.map Feed User.userIdParser
        , Url.map Home Url.top
        ]



-- INTERNAL --


pathFor : Route -> String
pathFor route =
    case route of
        Home ->
            "/"

        Feed userId ->
            "/" ++ User.userIdToString userId

        Search query ->
            Builder.relative [ "search" ] [ Builder.string "q" (Maybe.withDefault "" query) ]

        Login ->
            "/login"

        Logout ->
            "/logout"

        Register ->
            "/register"

        Item id ->
            "/item/" ++ Item.itemIdToString id

        EditItem id ->
            "/item/edit/" ++ Item.itemIdToString id

        NewItem ->
            "/item/new"

        NotFound ->
            "/"



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Html.Attributes.href (pathFor route)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (pathFor route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (pathFor route)


parseUrl : Url -> Route
parseUrl url =
    case parse matches url of
        Just route ->
            route

        Nothing ->
            NotFound
