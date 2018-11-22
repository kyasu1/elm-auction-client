module Page.Errored exposing (PageLoadError, pageLoadError, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.Page as Page exposing (ActivePage)



-- MODEL --


type PageLoadError
    = PageLoadError Model


type alias Model =
    { activePage : ActivePage
    , errorMessage : String
    }


pageLoadError : ActivePage -> String -> PageLoadError
pageLoadError activePage errorMessage =
    PageLoadError { activePage = activePage, errorMessage = errorMessage }


view : Session -> PageLoadError -> Html msg
view session (PageLoadError model) =
    section [ class "flex items-center justify-start flex-column flex-auto" ]
        [ article [ class "bl-ns br-ns pv3-ns ph4-ns b--light-gray w-90 mw8-ns flex-auto" ]
            [ h1 [] [ text "ページロードエラー" ]
            , p [] [ text model.errorMessage ]
            ]
        ]



-- type PageLoadError
--     = PageLoadError Model
--
--
-- type alias Model =
--     { activePage : ActivePage
--     , errorMessage : String
--     }
--
--
-- pageLoadError : ActivePage -> String -> PageLoadError
-- pageLoadError activePage errorMessage =
--     PageLoadError { activePage = activePage, errorMessage = errorMessage }
--
--
-- view : Session -> PageLoadError -> Html msg
-- view session (PageLoadError model) =
--     main_ [ class "" ]
--         [ h1 [] [ text "ページロードエラー" ]
--         , div []
--             [ p [] [ text model.errorMessage ]
--             ]
--         ]
