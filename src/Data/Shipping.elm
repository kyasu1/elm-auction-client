module Data.Shipping exposing
    ( Method(..)
    , Payer(..)
    , Size(..)
    , decoder
    , encoder
    , fromMethod
    , fromPayer
    , fromSize
    , toMethod
    , toSize
    )

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode


type Payer
    = Seller
    | Buyer Method


fromPayer : Payer -> String
fromPayer payer =
    case payer of
        Seller ->
            "出品者"

        Buyer _ ->
            "落札者"


type Method
    = Mail
    | Nekopos
    | Compact
    | Takkyubin Size


toMethod : String -> Method
toMethod method =
    case method of
        "普通郵便" ->
            Mail

        "ネコポス" ->
            Nekopos

        "コンパクト" ->
            Compact

        _ ->
            Debug.todo "Invalid method specified"


fromMethod : Method -> String
fromMethod method =
    case method of
        Mail ->
            "普通郵便"

        Nekopos ->
            "ネコポス"

        Compact ->
            "コンパクト"

        Takkyubin _ ->
            "宅急便"


type Size
    = Yamato60
    | Yamato80
    | Yamato100
    | Yamato120
    | Yamato140
    | Yamato160


toSize : String -> Size
toSize size =
    case size of
        "60サイズ" ->
            Yamato60

        "80サイズ" ->
            Yamato80

        "100サイズ" ->
            Yamato100

        "120サイズ" ->
            Yamato120

        "140サイズ" ->
            Yamato140

        "160サイズ" ->
            Yamato160

        _ ->
            Debug.todo "Invalid size specified"


fromSize : Size -> String
fromSize size =
    case size of
        Yamato60 ->
            "60サイズ"

        Yamato80 ->
            "80サイズ"

        Yamato100 ->
            "100サイズ"

        Yamato120 ->
            "120サイズ"

        Yamato140 ->
            "140サイズ"

        Yamato160 ->
            "160サイズ"



-- DECODER


decoder : Decoder Payer
decoder =
    field "payer" string
        |> andThen
            (\payer ->
                case payer of
                    "出品者" ->
                        succeed Seller

                    "落札者" ->
                        decoderWithBuyer

                    _ ->
                        fail <|
                            "Invalid payer specified"
            )


decoderWithBuyer : Decoder Payer
decoderWithBuyer =
    field "method" string
        |> andThen
            (\method ->
                case method of
                    "普通郵便" ->
                        succeed (Buyer Mail)

                    "ネコポス" ->
                        succeed (Buyer Nekopos)

                    "コンパクト" ->
                        succeed (Buyer Compact)

                    "宅急便" ->
                        decoderWithSize

                    _ ->
                        fail <|
                            "Invalid shipping method specified"
            )


decoderWithSize : Decoder Payer
decoderWithSize =
    field "size" string
        |> andThen
            (\size ->
                --              toSize size |> Takkyubin |> Buyer |> succeed
                case size of
                    "60サイズ" ->
                        succeed (Buyer <| Takkyubin Yamato60)

                    "80サイズ" ->
                        succeed (Buyer <| Takkyubin Yamato80)

                    "100サイズ" ->
                        succeed (Buyer <| Takkyubin Yamato100)

                    "120サイズ" ->
                        succeed (Buyer <| Takkyubin Yamato120)

                    "140サイズ" ->
                        succeed (Buyer <| Takkyubin Yamato140)

                    "160サイズ" ->
                        succeed (Buyer <| Takkyubin Yamato160)

                    _ ->
                        fail <|
                            "Invaid size specified"
            )



-- ENCODER


encoder : Payer -> Encode.Value
encoder payer =
    case payer of
        Seller ->
            Encode.object [ ( "payer", Encode.string "出品者" ) ]

        Buyer method ->
            let
                paidByBuyer =
                    ( "payer", Encode.string "落札者" )
            in
            case method of
                Mail ->
                    Encode.object [ paidByBuyer, ( "method", Encode.string "普通郵便" ) ]

                Nekopos ->
                    Encode.object [ paidByBuyer, ( "method", Encode.string "ネコポス" ) ]

                Compact ->
                    Encode.object [ paidByBuyer, ( "method", Encode.string "コンパクト" ) ]

                Takkyubin size ->
                    let
                        enc s =
                            Encode.object [ paidByBuyer, ( "method", Encode.string "宅急便" ), ( "size", Encode.string s ) ]
                    in
                    case size of
                        Yamato60 ->
                            enc "60サイズ"

                        Yamato80 ->
                            enc "80サイズ"

                        Yamato100 ->
                            enc "100サイズ"

                        Yamato120 ->
                            enc "120サイズ"

                        Yamato140 ->
                            enc "140サイズ"

                        Yamato160 ->
                            enc "160サイズ"
