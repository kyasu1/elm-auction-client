module Views.EditItem.Category exposing
    ( CategoryId
    , ExternalMsg(..)
    , Model
    , Msg
    , getCategoryId
    , getCategoryName
    , getCategoryPath
    , getModel
    , getSelected
    , update
    , view
    )

import Data.Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (class, classList, size)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Request.GraphQL.Error exposing (Error)
import Request.GraphQL.Query as Query
import Task exposing (Task)
import Views.Style as Style



-- MODEL --


type ChildCategory
    = Child (List Category)


type alias CategoryId =
    Int


type alias Category =
    { categoryId : CategoryId
    , categoryName : String
    , categoryPath : String
    , categoryIdPath : String
    , isLeaf : Bool
    , depth : Int
    , order : Int
    , isLink : Bool
    , isLeafToLink : Bool
    , childCategoryNum : Int
    , childCategory : ChildCategory
    }


type Model
    = Model InternalModel


type alias InternalModel =
    List ( Category, Maybe Category )



-- PUBLIC FUNCTIONS --


getSelected : Model -> Maybe Category
getSelected (Model model) =
    List.head model
        |> Maybe.andThen
            (\t ->
                case Tuple.second t of
                    Just _ ->
                        Nothing

                    Nothing ->
                        Just (Tuple.first t)
            )


getCategoryId : Category -> CategoryId
getCategoryId =
    .categoryId


getCategoryName : Category -> String
getCategoryName =
    .categoryName


getCategoryPath : Category -> String
getCategoryPath =
    .categoryPath


decoder : Decode.Decoder (List Category)
decoder =
    Decode.list decodeCategory


decodeCategory : Decode.Decoder Category
decodeCategory =
    Decode.succeed Category
        |> required "categoryId" Decode.int
        |> required "categoryName" Decode.string
        |> required "categoryPath" Decode.string
        |> required "categoryIdPath" Decode.string
        |> required "isLeaf" Decode.bool
        |> required "depth" Decode.int
        |> required "order" Decode.int
        |> required "isLink" Decode.bool
        |> required "isLeafToLink" Decode.bool
        |> optional "childCategoryNum" Decode.int 0
        |> optional "childCategory" decodeChildCategory (Child [])



-- https://github.com/elm-lang/elm-compiler/blob/0.18.0/hints/bad-recursion.md


decodeChildCategory : Decode.Decoder ChildCategory
decodeChildCategory =
    Decode.map Child (Decode.list (Decode.lazy (\_ -> decodeCategory)))


categoryFragment : String
categoryFragment =
    """
fragment categoryFragment on Category {
  categoryId
  categoryName
  categoryPath
  categoryIdPath
  parentCategoryId
  isLeaf
  depth
  order
  isLink
  isLeafToLink
  childCategoryNum
}
"""


getQuery : String
getQuery =
    categoryFragment ++ """
query getCategories($ids: CategoryIds) {
  categories(ids: $ids) {
    ...categoryFragment
    childCategory {
      ...categoryFragment
    }
  }
}
"""



-- VIEW --


view : Model -> Html Msg
view (Model model) =
    let
        depth =
            List.length model
    in
    div []
        [ div [ class "text-xl font-black" ] [ text "カテゴリ" ]
        , div [ class "f6" ]
            [ model
                |> List.head
                |> Maybe.map
                    (\( category, selected ) ->
                        case selected of
                            Just selected_ ->
                                selected_.categoryPath

                            Nothing ->
                                category.categoryPath
                    )
                |> Maybe.withDefault ""
                |> text
            ]
        , div [ class "flex flex-col md:flex-row" ]
            [ ul [ class "list-reset flex flex-col w-1/3" ]
                (model |> List.reverse |> List.map (categoryListView depth))
            , div [ class "w-full p-2" ]
                [ select [ class "w-full border", size 5 ]
                    [ option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    , option [] [ text "ここに最近使ったカテゴリが表示される予定です" ]
                    ]
                ]
            ]
        ]


categoryListView : Int -> ( Category, Maybe Category ) -> Html Msg
categoryListView depth ( category, selected ) =
    case ( category.isLeaf || category.isLeafToLink, category.childCategory ) of
        ( False, Child list ) ->
            li [ class "w-full mt-2 relative", onChange (Selected category) ]
                [ select [ class "block appearance-none w-full bg-white border border-grey-light hover:border-grey px-4 py-2 pr-8 rounded shadow leading-tight focus:outline-none focus:shadow-outline" ]
                    (List.append
                        [ option [ Html.Attributes.value "-1" ] [ text "---" ] ]
                        (List.map (options depth selected) list)
                    )
                , div [ class "pointer-events-none absolute pin-y pin-r flex items-center px-2 text-grey-darker" ]
                    [ i [ class "fas fa-angle-down" ] []
                    ]
                ]

        ( True, _ ) ->
            li [] []


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.on "change" (Decode.map tagger Html.Events.targetValue)


options : Int -> Maybe Category -> Category -> Html Msg
options depth selected category =
    case selected of
        Just selected_ ->
            option
                [ Html.Attributes.selected (selected_.categoryId == category.categoryId)
                , Html.Attributes.value <| String.fromInt category.categoryId
                , Html.Attributes.style "overflow" "hidden"
                , Html.Attributes.style "white-space" "nowrap"
                , Html.Attributes.style "text-overflow" "ellipsis"
                ]
                [ optionText depth category ]

        Nothing ->
            option [ Html.Attributes.value <| String.fromInt category.categoryId ] [ optionText depth category ]


optionText : Int -> Category -> Html Msg
optionText depth category =
    case ( category.isLeaf, category.isLink ) of
        ( _, True ) ->
            if depth > category.depth then
                span [] [ text <| category.categoryName ++ ">" ]

            else
                span [] [ text <| category.categoryName ++ "@" ]

        ( False, _ ) ->
            span [] [ text <| category.categoryName ++ ">" ]

        ( _, _ ) ->
            span [] [ text category.categoryName ]



-- UPDATE --


type Msg
    = UpdateCategory (Result Error Category)
    | UpdateCategories (Result Error InternalModel)
    | LoadCategory CategoryId
    | Selected Category String


type ExternalMsg
    = NoOp
    | SetCategory (Maybe ( CategoryId, String ))


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg (Model model) =
    let
        ( ( newModel, subMsg ), extMsg ) =
            updateInternal session msg model
    in
    ( ( Model newModel, subMsg ), extMsg )


updateInternal : Session -> Msg -> InternalModel -> ( ( InternalModel, Cmd Msg ), ExternalMsg )
updateInternal token msg model =
    let
        empty =
            ( ( model, Cmd.none ), NoOp )
    in
    case msg of
        UpdateCategory (Ok category) ->
            let
                list =
                    List.filter (\item -> .depth (Tuple.first item) < category.depth) model
            in
            ( ( ( category, Nothing ) :: list
              , Cmd.none
              )
            , NoOp
            )

        UpdateCategory (Err error) ->
            empty

        UpdateCategories (Ok categories) ->
            let
                extMsg =
                    categories
                        |> List.head
                        |> Maybe.andThen
                            (\( _, selected ) ->
                                case selected of
                                    Just category ->
                                        if category.isLeaf == True then
                                            Just ( category.categoryId, category.categoryPath )

                                        else
                                            Nothing

                                    Nothing ->
                                        Nothing
                            )
            in
            ( ( categories
              , Cmd.none
              )
            , SetCategory extMsg
            )

        UpdateCategories (Err error) ->
            empty

        LoadCategory id ->
            ( ( model, get token id ), NoOp )

        -- isLink :
        -- isLinkToLef :
        -- isLeaf :
        -- Else :
        --   Case depth of
        --     1 -> -- model[0]のみを残して
        -- 　    List.length model -
        Selected category optionId ->
            let
                maybeSelected =
                    String.toInt optionId
                        |> Maybe.andThen
                            (\id ->
                                case category.childCategory of
                                    Child categoryList ->
                                        categoryList
                                            |> List.filter
                                                (\e ->
                                                    e.categoryId == id
                                                )
                                            |> List.head
                            )
            in
            case maybeSelected of
                Just selected ->
                    if selected.isLink == True then
                        ( ( model, linkTo token selected.categoryId ), SetCategory Nothing )

                    else if selected.isLeaf == True then
                        let
                            dropped =
                                model
                                    |> List.drop (List.length model - category.depth - 1)

                            newModel =
                                dropped
                                    |> List.head
                                    |> Maybe.map (\( c, s ) -> ( c, maybeSelected ) :: List.drop 1 dropped)
                                    |> Maybe.withDefault dropped
                        in
                        ( ( newModel
                          , Cmd.none
                          )
                        , SetCategory (Just ( selected.categoryId, selected.categoryPath ))
                        )

                    else
                        ( ( List.map (setCategoryId (selected.depth - 1) selected) model
                          , get token selected.categoryId
                          )
                        , SetCategory Nothing
                        )

                -- ダミー行を選択した場合
                Nothing ->
                    let
                        dropped =
                            model
                                |> List.drop (List.length model - category.depth - 1)

                        newModel =
                            dropped
                                |> List.head
                                |> Maybe.map (\( c, s ) -> ( c, Nothing ) :: List.drop 1 dropped)
                                |> Maybe.withDefault dropped
                    in
                    ( ( newModel
                      , Cmd.none
                      )
                    , SetCategory Nothing
                    )


setSelected : List Category -> Maybe Category -> InternalModel
setSelected categories category =
    case categories of
        [] ->
            []

        head :: tail ->
            ( head, category ) :: setSelected tail (Just head)


setCategoryId : Int -> Category -> ( Category, Maybe Category ) -> ( Category, Maybe Category )
setCategoryId depth selected ( category, mybeSelected ) =
    if depth == category.depth then
        ( category, Just selected )

    else
        ( category, mybeSelected )


linkTo : Session -> CategoryId -> Cmd Msg
linkTo session id =
    getList session id
        |> Task.attempt UpdateCategories


get :
    Session
    -> CategoryId
    -> Cmd Msg
get session id =
    let
        variables =
            Encode.object
                --                [ ( "ids", Encode.int id |> List.singleton |> Encode.list )
                [ ( "ids", Encode.list Encode.int (List.singleton id) )
                ]
    in
    Query.send
        { session = session
        , query = Encode.string getQuery
        , variables = variables
        , field = "categories"
        , decoder = decoder
        }
        |> Task.andThen
            (\( categories, updatedSession ) ->
                case List.head categories of
                    Just category ->
                        Task.succeed category

                    Nothing ->
                        Task.fail (Request.GraphQL.Error.Error "Category not exist")
            )
        |> Task.attempt UpdateCategory


getModel : Session -> CategoryId -> Task Error Model
getModel session id =
    Task.map Model (getList session id)


getList : Session -> CategoryId -> Task Error InternalModel
getList session id =
    let
        variables =
            Encode.object
                [ ( "ids", Encode.list Encode.int (List.singleton id) )
                ]
    in
    -- find a category
    Query.send
        { session = session
        , query = Encode.string getQuery
        , variables = variables
        , field = "categories"
        , decoder = decoder
        }
        |> Task.andThen
            (\( categories, updatedSession ) ->
                case List.head categories of
                    Just category ->
                        let
                            ( offset, selected ) =
                                if category.isLeaf then
                                    ( 1, Just category )

                                else
                                    ( 0, Nothing )

                            ids =
                                category.categoryIdPath
                                    |> String.split ","
                                    |> List.map toInt
                                    |> List.reverse
                                    |> List.drop offset

                            newVariables =
                                Encode.object
                                    [ ( "ids", Encode.list Encode.int ids )
                                    ]
                        in
                        Query.send
                            { session = updatedSession
                            , query = Encode.string getQuery
                            , variables = newVariables
                            , field = "categories"
                            , decoder = decoder
                            }
                            |> Task.map
                                (\( listOfCategory, _ ) ->
                                    setSelected listOfCategory selected
                                )

                    Nothing ->
                        Task.fail <| Request.GraphQL.Error.Error "Category not exist"
            )


toInt : String -> Int
toInt str =
    String.toInt str
        |> Maybe.withDefault 0
