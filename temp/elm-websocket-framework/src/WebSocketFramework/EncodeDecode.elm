---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoder and decoder for WebSocketFramework
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.EncodeDecode
    exposing
        ( decodeMessage
        , decodePlist
        , decodeRawMessage
        , encodeMessage
        , messageDecoder
        , messageEncoder
        , rawMessageDecoder
        , rawMessageEncoder
        )

{-| The `EncodeDecode` module handles translation of strings sent over the wire to and from your message types.


# High-level functions

Your WebSocket subscription message handler will need to use `decodeMessage`, but you'll rarely directly use the rest of the functions in this module.

@docs decodeMessage, encodeMessage, messageDecoder, messageEncoder


# Low-level functions

@docs decodeRawMessage, rawMessageDecoder, rawMessageEncoder, decodePlist

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework.Types as Types
    exposing
        ( MessageDecoder
        , MessageEncoder
        , Plist
        , RawMessage
        , ReqRsp(..)
        , ServerInterface(..)
        )


-- Message


{-| Turn a string into a raw message.
-}
decodeRawMessage : String -> Result String RawMessage
decodeRawMessage string =
    JD.decodeString rawMessageDecoder string


{-| The `Decoder` used by `decodeRawMessage`
-}
rawMessageDecoder : Decoder RawMessage
rawMessageDecoder =
    JD.map3 RawMessage
        (JD.index 0 JD.string)
        (JD.index 1 JD.string)
        (JD.index 2 (JD.keyValuePairs JD.value))


{-| Decode "req" or "rsp" and a `message` name into either `Req message` or `Rsp message`.
-}
decodeReqRsp : String -> String -> Result String ReqRsp
decodeReqRsp reqrsp message =
    case reqrsp of
        "req" ->
            Ok <| Req message

        "rsp" ->
            Ok <| Rsp message

        _ ->
            Err <| "Expecting 'req' or 'rsp', got: '" ++ reqrsp ++ "'"


{-| User a `MessageDecoder` that you write to turn a string into a message.
-}
decodeMessage : MessageDecoder message -> String -> Result String message
decodeMessage decoder json =
    JD.decodeString (messageDecoder decoder) json


{-| Create a `Decoder` for `decodeMessage`.
-}
messageDecoder : MessageDecoder message -> Decoder message
messageDecoder decoder =
    rawMessageDecoder
        |> JD.andThen
            (\m ->
                case decodeReqRsp m.reqrsp m.msg of
                    Ok reqrsp ->
                        case decoder ( reqrsp, m.plist ) of
                            Ok message ->
                                JD.succeed message

                            Err msg ->
                                JD.fail msg

                    Err msg ->
                        JD.fail msg
            )


{-| Use a `MessageEncoder` that you write to turn a message into a string.
-}
encodeMessage : MessageEncoder message -> message -> String
encodeMessage encoder message =
    JE.encode 0 <| messageEncoder encoder message


{-| Use a `MessageEncoder` that you write to turn a message into a `Value`.

Rarely used by anything but `encodeMessage`.

-}
messageEncoder : MessageEncoder message -> message -> Value
messageEncoder encoder message =
    let
        ( reqrsp, plist ) =
            encoder message

        ( rr, msg ) =
            case reqrsp of
                Req m ->
                    ( "req", m )

                Rsp m ->
                    ( "rsp", m )
    in
    rawMessageEncoder
        { reqrsp = rr
        , msg = msg
        , plist = plist
        }


{-| Turn a raw message into a `Value`.
-}
rawMessageEncoder : RawMessage -> Value
rawMessageEncoder message =
    JE.list
        [ JE.string message.reqrsp
        , JE.string message.msg
        , JE.object message.plist
        ]


{-| Decode a list of key/value pairs into a message.
-}
decodePlist : Decoder message -> Plist -> Result String message
decodePlist decoder plist =
    JD.decodeValue decoder <| JE.object plist
