module Views.Page exposing (ActivePage(..), bodyId, frame)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Util


type ActivePage
    = Other
    | Home
    | NewItem
    | Login
    | Register


frame : Bool -> Maybe (User AuthToken) -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "container mx-auto min-h-screen flex flex-col" ]
        [ viewHeader page user isLoading
        , content
        , div [ id "clipboard" ] []
        , lazy2 Util.viewIf isLoading (div [ classList [ ( "loading", isLoading ) ] ] [])
        , viewFooter
        ]


viewHeader : ActivePage -> Maybe (User AuthToken) -> Bool -> Html msg
viewHeader page user isLoading =
    header [ class "px-4" ]
        [ div [ class "flex justify-between items-center py-4" ]
            [ div [ class "" ]
                [ a
                    [ class "no-underline text-black text-xl font-black"
                    , title "Home"
                    , Route.href Route.Home
                    ]
                    [ text "Auction Producer" ]
                ]
            , nav [ class "flex" ] <|
                navbarLink (page == Home) Route.Home [ text "Home" ]
                    :: viewSignIn page user
            ]
        ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    a
        [ classList
            [ ( "block no-underline text-black border-2 border-black p-2 mr-2 hover:bg-grey", True )
            , ( "text-blue font-bold", isActive )
            ]
        , Route.href route
        ]
        linkContent


viewSignIn : ActivePage -> Maybe (User AuthToken) -> List (Html msg)
viewSignIn page maybeUser =
    case maybeUser of
        Nothing ->
            [ navbarLink (page == Login) Route.Login [ text "ログイン" ]
            , navbarLink (page == Register) Route.Register [ text "ユーザー登録" ]
            ]

        Just user ->
            [ navbarLink (page == NewItem) Route.NewItem [ i [ class "" ] [], text "新規作成" ]
            , navbarLink False Route.Logout [ text "ログアウト" ]
            , div [ class "border-2 border-white p-2 mr-2" ] [ text user.name ]
            ]


viewFooter : Html msg
viewFooter =
    footer [ class "my-4" ]
        [ div [ class "text-center" ]
            [ a [ class "https://www.officeiko.co.jp" ] [ text "株式会社オフィスイコー" ]
            , text " Copyright 2017 Yasuyuki Komatsubara"
            ]
        ]


bodyId : String
bodyId =
    "page-body"
