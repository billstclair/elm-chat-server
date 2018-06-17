---------------------------------------------------------------------
--
-- Types.elm
-- Shared types for WebSocketFramework module.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , ErrorRsp
        , GameId
        , InputPort
        , MessageDecoder
        , MessageEncoder
        , MessageToGameid
        , ModeChecker
        , OutputPort
        , PlayerId
        , PlayerInfo
        , Plist
        , PublicGame
        , PublicGames
        , RawMessage
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        , ServerState
        , ServerUrl
        , emptyPublicGames
        , emptyServerState
        , printifyString
        )

{-| Types used by the rest of the WebSocketFramework modules.


# State

@docs ServerState, ServerInterface, PlayerInfo, PublicGame, PublicGames


# Empty states

@docs emptyServerState, emptyPublicGames


# Messages

@docs RawMessage, ReqRsp, Plist, ErrorRsp


# Function Signatures

@docs ServerMessageProcessor, MessageToGameid, ModeChecker


# Message Encoding/Decoding

@docs MessageDecoder, MessageEncoder, EncodeDecode


# Utility

@docs printifyString


# Server Ports

@docs InputPort, OutputPort


# Aliases

@docs GameId, PlayerId, ServerUrl

-}

import Char
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import String.Extra as SE


{-| An alias for `String`.
-}
type alias ServerUrl =
    String


{-| An alias for `String`.
-}
type alias GameId =
    String


{-| An alias for `String`.
-}
type alias PlayerId =
    String


{-| A list of key/value pairs.
-}
type alias Plist =
    List ( String, Value )


{-| A raw request, ready to be encoded to go over the wire.

`reqrsp` is "req" or "rsp" for request or response.

`msg` is the name of the message.

`plist` is the parameters of the message.

-}
type alias RawMessage =
    { reqrsp : String
    , msg : String
    , plist : Plist
    }


{-| An error response that encapsulates a request message and an error string.
-}
type alias ErrorRsp message =
    { request : message
    , text : String
    }


{-| A type safe representation of the `reqrsp` and `msg` fields of a `RawMessage`.
-}
type ReqRsp
    = Req String
    | Rsp String


{-| Type signature for a function that turns a request or response and a key/value list into a message.
-}
type alias MessageDecoder message =
    ( ReqRsp, Plist ) -> Result String message


{-| Type signature for a function that turns a message into request or response and a key/value list.
-}
type alias MessageEncoder message =
    message -> ( ReqRsp, Plist )


{-| Wrapping of encoder, decoder, and error message creator.
-}
type alias EncodeDecode message =
    { encoder : MessageEncoder message
    , decoder : MessageDecoder message
    , errorWrapper : Maybe (String -> message)
    }


{-| Type signature of a function that checks if a message is a legal request for a gamestate.

An `Err` result has a message string. An `Ok` result maps an unusable value.

-}
type alias ModeChecker gamestate message =
    gamestate -> message -> Result String Never


{-| Type signature of a function which turns a server state and a request message into a ServerState and maybe a response message.
-}
type alias ServerMessageProcessor gamestate player message =
    ServerState gamestate player -> message -> ( ServerState gamestate player, Maybe message )


{-| Type signature of a function that extracts the game id from a message, if it has one.
-}
type alias MessageToGameid message =
    message -> Maybe GameId


{-| Information about a player in a game
-}
type alias PlayerInfo player =
    { gameid : GameId
    , player : player
    }


{-| If your server supports public games, this represents the game id and name of the creator of the game.
-}
type alias PublicGame =
    { gameid : GameId
    , playerName : String
    }


{-| A list of pubic games.
-}
type alias PublicGames =
    List PublicGame


{-| An empty list of public games.
-}
emptyPublicGames : PublicGames
emptyPublicGames =
    []


{-| The part of the server state that is independent from its socket connections.

You might think that the names are per game, and you'd be right to think that.
They're not in the GameState, because they don't need to go over the wire,
except in the JoinRsp.

They're stored in a Dict in the Server model.

-}
type alias ServerState gamestate player =
    { gameDict : Dict GameId gamestate
    , playerDict : Dict PlayerId (PlayerInfo player)
    , publicGames : PublicGames
    , state : Maybe gamestate --used by servers with no concept of game
    }


{-| Create a mostly empty `ServerState`.
-}
emptyServerState : Maybe gamestate -> ServerState gamestate player
emptyServerState gamestate =
    { gameDict = Dict.empty
    , playerDict = Dict.empty
    , publicGames = emptyPublicGames
    , state = gamestate
    }


{-| Everything necessary to communicate with a server, be it real or proxy.
-}
type ServerInterface gamestate player message msg
    = ServerInterface
        { server : ServerUrl
        , wrapper : ServerInterface gamestate player message msg -> message -> msg
        , state : Maybe (ServerState gamestate player)
        , sender : ServerInterface gamestate player message msg -> message -> Cmd msg
        }


{-| The input port from the Node.js code to the WebSocket server.
-}
type alias InputPort msg =
    (Value -> msg) -> Sub msg


{-| The output port from the WebSocket server to the Node.js code.
-}
type alias OutputPort msg =
    Value -> Cmd msg


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


closingQuote : String
closingQuote =
    stringFromCode 0x201D


{-| Convert ASCII double-quote characters to curly end quotes.
-}
printifyString : String -> String
printifyString string =
    SE.replace "\"" closingQuote string
