port module PortMessage exposing
    ( PortMessage
    , new, withPayload
    , elmToJs, jsToElm
    )

{-| Useful functions for building a PortMessage to be delivered through ports.

@docs PortMessage


# Builder

@docs new, withPayload

-}

import Json.Encode as Encode


{-| -}
type alias PortMessage =
    { tag : String
    , payload : Encode.Value
    }


{-| Create a new PortMessage with an empty payload.

    new "SomeAction" == { tag = "SomeAction", payload = null }

-}
new : String -> PortMessage
new tag =
    PortMessage tag Encode.null


{-| Attach a payload to the PortMessage

    new "JoinChannel"
        |> withPayload (Encode.string "lobby")
        == { tag = "JoinChannel", payload = "lobby" }

-}
withPayload : Encode.Value -> PortMessage -> PortMessage
withPayload payload message =
    { message | payload = payload }


port elmToJs : PortMessage -> Cmd a


port jsToElm : (Encode.Value -> msg) -> Sub msg
