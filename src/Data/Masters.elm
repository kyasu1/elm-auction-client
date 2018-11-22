module Data.Masters exposing
    ( MasterList
    , Masters
    , decoder
    , getList
    , getQuery
    , initial
    , lookup
    , lookupText
    , toTuple
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, field, int, list, nullable, string)
import Json.Decode.Pipeline as Pipeline exposing (custom, hardcoded, required)



-- MODEL --


type Masters
    = Masters (Dict String (List Master))


type alias MasterList =
    List ( String, List Master )



-- Internal Data --


type alias Master =
    { id : String
    , text : String
    }


mastersWithConfig :
    { conditions : List Master
    , package_sizes : List Master
    , package_weights : List Master
    , payment_methods : List Master
    , selling_types : List Master
    , shipping_methods : List Master
    , shipping_paid_bys : List Master
    }
    -> MasterList
mastersWithConfig config =
    [ ( "conditions", config.conditions )
    , ( "packageSizes", config.package_sizes )
    , ( "packageWeights", config.package_weights )
    , ( "paymentMethods", config.payment_methods )
    , ( "sellingTypes", config.selling_types )
    , ( "shippingMethods", config.shipping_methods )
    , ( "shippingPaidBys", config.shipping_paid_bys )
    ]


initial : Masters
initial =
    Masters <|
        Dict.fromList
            (mastersWithConfig
                { conditions = []
                , package_sizes = []
                , package_weights = []
                , payment_methods = []
                , selling_types = []
                , shipping_methods = []
                , shipping_paid_bys = []
                }
            )



-- PUBLIC FUNCTIONS --


toTuple : String -> Masters -> List ( String, String )
toTuple key masters =
    List.map (\master -> ( master.id, master.text )) (getList key masters)


lookupText : String -> String -> Masters -> Maybe String
lookupText key id masters =
    getList key masters
        |> List.filter (\master -> master.id == id)
        |> List.head
        |> Maybe.map .text



-- PRIVATE FUNCTIONS --


getList : String -> Masters -> List Master
getList key (Masters masters) =
    case Dict.get key masters of
        Just list ->
            list

        Nothing ->
            []


lookup : String -> String -> Masters -> Master
lookup key id (Masters masters) =
    case Dict.get key masters of
        Just list ->
            list
                |> List.filter (\item -> item.id == id)
                |> List.head
                |> Maybe.withDefault (Master "" "")

        Nothing ->
            Master "" ""


decoder : Decoder Masters
decoder =
    Decode.map Masters
        (Decode.dict (list masterDecoder))


masterDecoder : Decoder Master
masterDecoder =
    Decode.succeed Master
        |> required "id" string
        |> required "text" string


getQuery : String
getQuery =
    """
  query getMasters {
    conditions {
      id
      text
    }
    package_sizes {
      id
      text
    }
    package_weights {
      id
      text
    }
    payment_methods {
      id
      text
    }
    selling_types {
      id
      text
    }
    shipping_methods {
      id
      text
    }
    shipping_paid_bys {
      id
      text
    }
  }
"""
