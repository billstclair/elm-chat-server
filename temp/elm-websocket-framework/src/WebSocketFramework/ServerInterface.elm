----------------------------------------------------------------------
--
-- ServerInterface.elm
-- WebSocketFramework server implementation.
-- Usable locally on the client, or via WebSocket on a server.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.ServerInterface
    exposing
        ( appendPublicGames
        , checkGameid
        , checkOnlyGameid
        , checkPlayerid
        , dummyGameid
        , errorRsp
        , fullMessageProcessor
        , getServer
        , makeProxyServer
        , makeServer
        , removePublicGame
        , send
        )

{-| Functions that connect the client code to the server.


# Server Constructors

@docs makeServer, makeProxyServer, fullMessageProcessor


# Sending a Message

@docs send


# State Access

@docs getServer


# GameId and PlayerId validity checking

@docs checkOnlyGameid, checkGameid, checkPlayerid, dummyGameid


# Public Games

@docs appendPublicGames, removePublicGame


# Errors

@docs errorRsp

-}

import Debug exposing (log)
import Dict exposing (Dict)
import List.Extra as LE
import Task
import WebSocket
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)
import WebSocketFramework.Types as Types
    exposing
        ( EncodeDecode
        , ErrorRsp
        , GameId
        , MessageEncoder
        , ModeChecker
        , PlayerId
        , PlayerInfo
        , PublicGame
        , PublicGames
        , RawMessage
        , ServerInterface(..)
        , ServerMessageProcessor
        , ServerState
        , ServerUrl
        , emptyPublicGames
        , emptyServerState
        , printifyString
        )


{-| `"<gameid>"`
-}
dummyGameid : GameId
dummyGameid =
    "<gameid>"


{-| Make a client connection to a proxy server.

No WebSocket connection will be used to send messages.

-}
makeProxyServer : ServerMessageProcessor gamestate player message -> (ServerInterface gamestate player message msg -> message -> msg) -> ServerInterface gamestate player message msg
makeProxyServer messageProcessor wrapper =
    ServerInterface
        { server = ""
        , wrapper = wrapper
        , state = Nothing
        , sender = proxySender messageProcessor
        }


{-| Simulate a round-trip through the message encoder, decoder, and message processor.

Returns a function that takes a request message, encodes it, decodes it, processes it into a response, encodes and decodes that, then returns the result.

Usually passed as the first arg to `makeProxyServer`.

-}
fullMessageProcessor : EncodeDecode message -> ServerMessageProcessor gamestate player message -> ServerMessageProcessor gamestate player message
fullMessageProcessor encodeDecode messageProcessor state message =
    let
        err =
            \msg ->
                case encodeDecode.errorWrapper of
                    Nothing ->
                        Nothing

                    Just wrapper ->
                        Just <| wrapper msg

        req =
            encodeMessage encodeDecode.encoder message

        dbg =
            log "fullMessageProcesor, req" <|
                printifyString req
    in
    case decodeMessage encodeDecode.decoder req of
        Err msg ->
            ( state, err msg )

        Ok message2 ->
            let
                ( state2, rspmsg ) =
                    messageProcessor state message2

                message3 =
                    case rspmsg of
                        Nothing ->
                            Nothing

                        Just r ->
                            let
                                rsp =
                                    encodeMessage encodeDecode.encoder r

                                dbg2 =
                                    log "  rsp" <|
                                        printifyString rsp
                            in
                            case decodeMessage encodeDecode.decoder rsp of
                                Err msg ->
                                    err msg

                                Ok m ->
                                    Just m
            in
            ( state2, message3 )


{-| Make a client connection to a real WebSocket server.

The `msg` will usually be a no-operation message. It is only used to fill a slot in the returned `ServerInterface`. That slot is only used by the proxy server.

-}
makeServer : MessageEncoder message -> ServerUrl -> msg -> ServerInterface gamestate player message msg
makeServer encoder server msg =
    ServerInterface
        { server = server
        , wrapper = \_ _ -> msg
        , state = Nothing
        , sender = sender encoder
        }


{-| Return the server URL from inside a ServerInterface.
-}
getServer : ServerInterface gamestate player message msg -> ServerUrl
getServer (ServerInterface interface) =
    interface.server


{-| Return a `Cmd` to send a message through a server interface.
-}
send : ServerInterface gamestate player message msg -> message -> Cmd msg
send ((ServerInterface interface) as si) message =
    interface.sender si message


proxyCmd : ServerInterface gamestate player message msg -> message -> Cmd msg
proxyCmd ((ServerInterface interface) as si) message =
    let
        task =
            Task.succeed message

        wrapper =
            interface.wrapper si
    in
    Task.perform wrapper task


proxySender : ServerMessageProcessor gamestate player message -> ServerInterface gamestate player message msg -> message -> Cmd msg
proxySender processor (ServerInterface interface) message =
    let
        state =
            Maybe.withDefault (emptyServerState Nothing) interface.state

        ( s2, return ) =
            processor state message
    in
    case return of
        Nothing ->
            Cmd.none

        Just m ->
            proxyCmd (ServerInterface { interface | state = Just s2 }) m


sender : MessageEncoder message -> ServerInterface gamestate player message msg -> message -> Cmd msg
sender encoder (ServerInterface interface) message =
    WebSocket.send interface.server (encodeMessage encoder message)


{-| Create the ErrorRsp record returned in the errors from `CheckOnlyGameid`, `checkGameid`, and `checkPlayerid`.
-}
errorRsp : message -> String -> ErrorRsp message
errorRsp message text =
    { request = message
    , text = text
    }


{-| Check that the passed `GameId` is in the `ServerState`'s game dict.

If it is, return the `gamestate`. Otherwise wrap the message in an `ErrorRsp`.

-}
checkOnlyGameid : ServerState gamestate player -> message -> GameId -> Result (ErrorRsp message) gamestate
checkOnlyGameid state message gameid =
    case Dict.get gameid state.gameDict of
        Just gameState ->
            Ok gameState

        Nothing ->
            Err <| errorRsp message "Unknown gameid"


{-| Check that the passed `PlayerId` is in the `ServerState`'s player dict.

If it is, return the `PlayerInfo` record for the player.

Otherwise wrap the message in an `ErrorRsp`.

-}
checkPlayerid : ServerState gamestate player -> message -> PlayerId -> Result (ErrorRsp message) (PlayerInfo player)
checkPlayerid state message playerid =
    case Dict.get playerid state.playerDict of
        Nothing ->
            Err <| errorRsp message ("Unknown playerid " ++ playerid)

        Just info ->
            Ok info


{-| Check that the passed `GameId` is in the `ServerState`'s game dict and that it satisifed the `ModeChecker`.

If it does, return the `gamestate`. Otherwise wrap the message in an `ErrorRsp`.

-}
checkGameid : ModeChecker gamestate message -> ServerState gamestate player -> message -> GameId -> Result (ErrorRsp message) gamestate
checkGameid checker state message gameid =
    case checkOnlyGameid state message gameid of
        Ok gamestate ->
            case checker gamestate message of
                Ok _ ->
                    Ok gamestate

                Err msg ->
                    Err (errorRsp message msg)

        err ->
            err


{-| Push a `PublicGame` onto a list of them.
-}
appendPublicGames : PublicGame -> PublicGames -> PublicGames
appendPublicGames game games =
    List.append games [ game ]


{-| Remove the `PublicGame` with the given `GameId` from a list of games.
-}
removePublicGame : GameId -> PublicGames -> PublicGames
removePublicGame gameid games =
    List.filter (\game -> game.gameid /= gameid) games
