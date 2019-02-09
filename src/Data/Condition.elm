module Data.Condition exposing (Condition(..), decoder, encoder, toString)

import Json.Decode exposing (Decoder, andThen, fail, string, succeed)
import Json.Encode as Encode exposing (Value)


type Condition
    = New
    | RankS
    | RankA
    | RankB
    | RankC
    | RankD


toString : Condition -> String
toString cond =
    case cond of
        New ->
            "1"

        RankS ->
            "4"

        RankA ->
            "5"

        RankB ->
            "6"

        RankC ->
            "7"

        RankD ->
            "8"


decoder : Decoder Condition
decoder =
    string
        |> andThen
            (\condition ->
                case condition of
                    "1" ->
                        succeed New

                    "4" ->
                        succeed RankS

                    "5" ->
                        succeed RankA

                    "6" ->
                        succeed RankB

                    "7" ->
                        succeed RankC

                    "8" ->
                        succeed RankD

                    "2" ->
                        succeed RankB

                    "3" ->
                        succeed RankD

                    _ ->
                        fail <|
                            "Failed decode condition field"
            )


encoder : Condition -> Value
encoder condition =
    condition |> toString |> Encode.string
