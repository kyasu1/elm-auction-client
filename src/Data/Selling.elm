module Data.Selling exposing (Selling(..), decoder, encoder)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode


type alias PriceOpen =
    Int


type alias PriceBuyout =
    Int


type alias AllowDiscount =
    Bool


type Selling
    = Auction PriceOpen PriceBuyout
    | Fixed PriceBuyout AllowDiscount


decoder : Decoder Selling
decoder =
    field "method" string
        |> andThen
            (\method ->
                case method of
                    "オークション形式" ->
                        auctionDecoder

                    "即決形式" ->
                        fixedDecoder

                    _ ->
                        fail <|
                            "Invalid selling method specified"
            )


auctionDecoder : Decoder Selling
auctionDecoder =
    map2 Auction
        (field "priceOpen" int)
        (field "priceBuyout" int)


fixedDecoder : Decoder Selling
fixedDecoder =
    map2 Fixed
        (field "priceBuyout" int)
        (field "allowDiscount" bool)


encoder : Selling -> Encode.Value
encoder selling =
    case selling of
        Auction priceOpen priceBuyout ->
            Encode.object
                [ ( "method", Encode.string "オークション形式" )
                , ( "priceOpen", Encode.int priceOpen )
                , ( "priceBuyout", Encode.int priceBuyout )
                ]

        Fixed priceBuyout allowDiscount ->
            Encode.object
                [ ( "method", Encode.string "即決形式" )
                , ( "priceBuyout", Encode.int priceBuyout )
                , ( "allowDiscount", Encode.bool allowDiscount )
                ]
