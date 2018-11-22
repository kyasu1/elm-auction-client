module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Session exposing (Session)
import Data.User as User exposing (User)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page.EditItem as EditItem
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Item as Item
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Ports
import Request.GraphQL.Error
import Route exposing (Route)
import Task
import Url exposing (Url)
import Views.Page as Page


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home Home.Model
    | Login Login.Model
    | Register Register.Model
    | Item Item.Model
    | ItemEditor EditItem.Model


type PageState
    = Loaded Page
    | TransitionFrom Page



-- MODEL --


type alias Model =
    { session : Session
    , pageState : PageState
    }


type alias Flags =
    { session : Maybe Value
    }


decodeUserFromJson : Value -> Maybe (User AuthToken)
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString (Decode.field "session" User.decoderWithToken) >> Result.toMaybe)


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case decodeUserFromSession flags.session of
        Just user ->
            setRoute (Route.parseUrl url)
                { session = { user = Just user, key = key }
                , pageState = Loaded initialPage
                }

        Nothing ->
            ( { pageState = Loaded initialPage, session = { user = Nothing, key = key } }
            , Route.pushUrl key Route.Login
            )


decodeUserFromSession : Maybe Value -> Maybe (User AuthToken)
decodeUserFromSession maybeSession =
    maybeSession |> Maybe.andThen (\session -> decodeUserFromJson session)


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.pageState of
                Loaded page ->
                    viewPage model.session False page

                TransitionFrom page ->
                    viewPage model.session True page
    in
    { title = "App"
    , body = [ body ]
    }


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user
    in
    case page of
        Blank ->
            Html.text ""
                |> frame Page.Other

        NotFound ->
            NotFound.view session
                |> frame Page.Other

        Errored subModel ->
            Errored.view session subModel
                |> frame Page.Other

        Home subModel ->
            Home.view session subModel
                |> frame Page.Home
                |> Html.map HomeMsg

        Item subModel ->
            Item.view session subModel
                |> frame Page.Other
                |> Html.map ItemMsg

        ItemEditor subModel ->
            case session.user of
                Just user ->
                    EditItem.view subModel
                        |> frame Page.Other
                        |> Html.map EditItemMsg

                Nothing ->
                    text "MUST BE LOGIN"

        Login subModel ->
            Login.view session subModel
                |> frame Page.Other
                |> Html.map LoginMsg

        Register subModel ->
            Register.view session subModel
                |> frame Page.Other
                |> Html.map RegisterMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.getInfoFromOutside Outside LogError
        , case getPage model.pageState of
            Home homeModel ->
                Sub.map HomeMsg (Home.subscriptions homeModel)

            _ ->
                Sub.none

        -- Sub.map SetUser sessionChange
        ]



-- sessionChange : Sub (Maybe (User AuthToken))
-- sessionChange =
--     Ports.onSessionChange (Decode.decodeValue (User.decoderWithToken "session") >> Result.toMaybe)
-- date : Sub (Maybe Date)
-- date =
--     Ports.date (Decode.decodeValue Decode.date >> Result.toMaybe)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitionFrom page ->
            page



-- UPDATE --


type Msg
    = SetRoute Route
    | HomeLoaded (Result PageLoadError ( Home.Model, Session ))
    | ItemEditorLoaded (Result PageLoadError ( EditItem.Model, Session ))
    | ItemViewLoaded (Result PageLoadError ( Item.Model, Session ))
    | LoginLoaded (Result Request.GraphQL.Error.Error Login.Model)
    | HomeMsg Home.Msg
    | ItemMsg Item.Msg
    | EditItemMsg EditItem.Msg
    | SetUser (Maybe (User AuthToken))
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | Outside Ports.InfoForElm
    | LogError String
    | OnUrlChange Url
    | OnUrlRequest UrlRequest


setRoute : Route -> Model -> ( Model, Cmd Msg )
setRoute route model =
    let
        transition toMsg task =
            ( { model | pageState = TransitionFrom (getPage model.pageState) }
            , Task.attempt toMsg task
            )

        errored =
            pageErrored model
    in
    case route of
        Route.NotFound ->
            ( { model | pageState = Loaded NotFound }, Cmd.none )

        Route.Home ->
            transition HomeLoaded (Home.initialTask model.session Nothing Nothing)

        Route.Feed userId ->
            transition HomeLoaded (Home.initialTask model.session Nothing (Just userId))

        Route.Search maybeQuery ->
            transition HomeLoaded (Home.initialTask model.session maybeQuery Nothing)

        Route.Login ->
            transition LoginLoaded Login.initialTask

        Route.Logout ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | user = Nothing } }
            , Cmd.batch [ Ports.sendInfoOutside Ports.ClearSession, Route.pushUrl session.key Route.Login ]
              -- , Cmd.batch [ Ports.storeSession Nothing, Route.pushUrl session.key Route.Login ]
            )

        Route.Register ->
            ( { model | pageState = Loaded (Register Register.initialModel) }, Cmd.none )

        Route.Item itemId ->
            transition ItemViewLoaded (Item.initialTask model.session itemId)

        Route.NewItem ->
            case model.session.user of
                Just user ->
                    transition ItemEditorLoaded (EditItem.initialNewTask user model.session)

                Nothing ->
                    errored Page.NewItem "新規作成するためにはログインする必要があります"

        Route.EditItem itemId ->
            transition ItemEditorLoaded (EditItem.initialEditTask model.session itemId)



-- case model.session.user of
--     Just user ->
--         transition ItemEditorLoaded (EditItem.initialEditTask model.session itemId)
--
--     Nothing ->
--         errored Page.NewItem "編集するためにはログインする必要があります"


pageErrored : Model -> Page.ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
    ( { model | pageState = Loaded (Errored error) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
    in
    case ( msg, page ) of
        ( SetRoute route, ItemEditor itemModel ) ->
            if itemModel.editting == True then
                case itemModel.itemId of
                    Just itemId ->
                        -- (model, Route.modfiyUrl (Route.EditeItem itemId))
                        ( model, Ports.sendInfoOutside <| Ports.ReplaceState (Route.pathFor <| Route.EditItem itemId) )

                    Nothing ->
                        -- (model, Route.modfiyUrl (Route.NewItem))
                        ( model, Ports.sendInfoOutside <| Ports.ReplaceState (Route.pathFor Route.NewItem) )

            else
                setRoute route model

        ( SetRoute route, _ ) ->
            setRoute route model

        ( SetUser user, _ ) ->
            let
                cmd =
                    if session.user /= Nothing && user == Nothing then
                        Route.pushUrl session.key Route.Home

                    else
                        Cmd.none
            in
            ( { model | session = { session | user = user } }, cmd )

        ( LoginLoaded response, _ ) ->
            case response of
                Ok pageModel ->
                    ( { model | pageState = Loaded (Login pageModel) }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Login.update session subMsg subModel

                newModel =
                    case msgFromPage of
                        Login.NoOp ->
                            model

                        Login.SetUser user ->
                            { model | session = { session | user = Just user } }
            in
            ( { newModel | pageState = Loaded (Login pageModel) }, Cmd.map LoginMsg cmd )

        ( RegisterMsg subMsg, Register subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Register.update session subMsg subModel

                newModel =
                    case msgFromPage of
                        Register.NoOp ->
                            model

                        Register.SetUser user ->
                            { model | session = { session | user = Just user } }
            in
            ( { newModel | pageState = Loaded (Register pageModel) }, Cmd.map RegisterMsg cmd )

        ( HomeLoaded response, _ ) ->
            case response of
                Ok ( pageModel, updatedSession ) ->
                    ( { model
                        | pageState = Loaded (Home pageModel)
                        , session = updatedSession
                      }
                    , Cmd.none
                    )

                Err err ->
                    case Debug.log "err" err of
                        _ ->
                            let
                                updatedSession =
                                    { session | user = Nothing }
                            in
                            ( { model | session = updatedSession }
                            , Cmd.batch [ Ports.sendInfoOutside Ports.ClearSession, Route.pushUrl session.key Route.Login ]
                            )

        ( HomeMsg subMsg, Home subModel ) ->
            let
                ( pageModel, updatedSession, cmd ) =
                    Home.update model.session subMsg subModel
            in
            ( { model | pageState = Loaded (Home pageModel), session = updatedSession }
            , Cmd.map HomeMsg cmd
            )

        ( ItemMsg subMsg, Item subModel ) ->
            let
                ( pageModel, updatedSession, cmd ) =
                    Item.update model.session subMsg subModel
            in
            ( { model | pageState = Loaded (Item pageModel), session = updatedSession }
            , Cmd.map ItemMsg cmd
            )

        ( ItemViewLoaded response, _ ) ->
            case response of
                Ok ( pageModel, updatedSession ) ->
                    ( { model
                        | pageState = Loaded (Item pageModel)
                        , session = updatedSession
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | pageState = Loaded (Errored err) }
                    , Ports.sendInfoOutside Ports.ClearSession
                    )

        ( ItemEditorLoaded response, _ ) ->
            case response of
                Ok ( pageModel, updatedSession ) ->
                    ( { model
                        | pageState = Loaded (ItemEditor pageModel)
                        , session = updatedSession
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | pageState = Loaded (Errored err) }
                    , Ports.sendInfoOutside Ports.ClearSession
                    )

        ( EditItemMsg subMsg, ItemEditor subModel ) ->
            case session.user of
                Just user ->
                    let
                        ( pageModel, updatedSession, cmd ) =
                            EditItem.update user session subMsg subModel
                    in
                    ( { model | pageState = Loaded (ItemEditor pageModel), session = updatedSession }
                    , Cmd.map EditItemMsg cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        ( Outside infoForElm, _ ) ->
            case infoForElm of
                Ports.SessionChange newUser ->
                    let
                        updatedSession =
                            { session | user = newUser }
                    in
                    ( { model | session = updatedSession }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( LogError error, _ ) ->
            ( model, Ports.sendInfoOutside (Ports.LogError error) )

        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl session.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( OnUrlChange url, _ ) ->
            setRoute (Route.parseUrl url) model

        ( _, _ ) ->
            ( model, Cmd.none )



-- MAIN --


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
