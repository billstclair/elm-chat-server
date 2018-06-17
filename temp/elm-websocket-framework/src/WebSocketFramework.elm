---------------------------------------------------------------------
--
-- WebSocketFramework.elm
-- Most-used functions for WebSocket client/server framework.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework
    exposing
        ( decodePlist
        , makeProxyServer
        , makeServer
        , send
        , unknownMessage
        )

{-| Expose the most-used functions from `WebSocketFramework.ServerInterface`.


# Constructors

@docs makeServer, makeProxyServer


# Functions

@docs decodePlist, send, unknownMessage

-}

import Json.Decode as JD exposing (Decoder)
import WebSocketFramework.EncodeDecode as EncodeDecode
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( MessageEncoder
        , Plist
        , ReqRsp(..)
        , ServerInterface
        , ServerMessageProcessor
        , ServerUrl
        )


{-| Make a client connection to a proxy server.

No WebSocket connection will be used to send messages.

-}
makeProxyServer : ServerMessageProcessor gamestate player message -> (ServerInterface gamestate player message msg -> message -> msg) -> ServerInterface gamestate player message msg
makeProxyServer =
    ServerInterface.makeProxyServer


{-| Make a client connection to a real WebSocket server.

The `msg` will usually be a no-operation message. It is only used to fill a slot in the returned `ServerInterface`. That slot is only used by the proxy server.

-}
makeServer : MessageEncoder message -> ServerUrl -> msg -> ServerInterface gamestate player message msg
makeServer =
    ServerInterface.makeServer


{-| Return a `Cmd` to send a message through a server interface.
-}
send : ServerInterface gamestate player message msg -> message -> Cmd msg
send =
    ServerInterface.send


{-| Decode a list of key/value pairs into a message.
-}
decodePlist : Decoder message -> Plist -> Result String message
decodePlist =
    EncodeDecode.decodePlist


{-| Return an `Err` reporting on an unknown message name.
-}
unknownMessage : ReqRsp -> Result String message
unknownMessage reqrsp =
    let
        ( typ, message ) =
            case reqrsp of
                Req m ->
                    ( "request", m )

                Rsp m ->
                    ( "response", m )

        msg =
            "Unknown " ++ typ ++ " message: '" ++ message ++ "'"
    in
    Err msg
