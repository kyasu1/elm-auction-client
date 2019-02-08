module Page.Home exposing (Model, Msg, initialTask, subscriptions, update, view)

import Data.Item.Feed as Feed
import Data.Session as Session exposing (Session)
import Data.User exposing (UserId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Item exposing (ListConfig, defaultListConfig)
import Task exposing (Task)
import Views.Item.Feed as Feed
import Views.Page as Page



-- MODEL --


type alias Model =
    { feed : Feed.Model
    }


initialTask : Session -> Maybe String -> Maybe UserId -> Task PageLoadError ( Model, Session )
initialTask session maybeSearch maybeUserId =
    let
        config : ListConfig
        config =
            { defaultListConfig | query = maybeSearch, userId = maybeUserId }

        handleLoadError : a -> PageLoadError
        handleLoadError e =
            -- pageLoadError Page.Other ("一覧ページが読み込めませんでした..." ++ Debug.toString e)
            pageLoadError Page.Other "一覧ページが読み込めませんでした..."
    in
    Task.map (\( model, updatedSession ) -> ( Model model, updatedSession )) (Feed.initialTask session config)
        |> Task.mapError handleLoadError


view : Session -> Model -> Html Msg
view session model =
    section [ class "mx-auto" ]
        [ Feed.view session model.feed |> Html.map FeedMsg
        ]



-- UPDATE --


type Msg
    = FeedMsg Feed.Msg


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update session msg model =
    case msg of
        FeedMsg subMsg ->
            let
                ( newFeed, updatedSession, subCmd ) =
                    Feed.update session subMsg model.feed
            in
            ( { model | feed = newFeed }, updatedSession, Cmd.map FeedMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map FeedMsg (Feed.subscriptions model.feed)
