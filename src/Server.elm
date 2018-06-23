----------------------------------------------------------------------
--
-- Server.elm
-- The top-level file for the ChatClient server.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Note: This file will NOT compile against the elm-package.json in
-- the top-level directory. It WILL compile against the elm-package.json
-- in the `server` directory.
--
----------------------------------------------------------------------


port module Server exposing (..)

import ChatClient.EncodeDecode
    exposing
        ( messageDecoder
        , messageEncoder
        )
import ChatClient.Interface exposing (messageProcessor)
import ChatClient.Types
    exposing
        ( ErrorKind(..)
        , GameState
        , Message(..)
        , Player
        )
import Debug exposing (log)
import Dict
import List.Extra as LE
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , Socket
        , UserFunctions
        , WrappedModel(..)
        , newGameid
        , newPlayerid
        , sendToMany
        , sendToOne
        , verbose
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , Error
        , GameId
        , InputPort
        , OutputPort
        , PlayerId
        , ServerState
        )


type alias ServerModel =
    ()


serverModel : ServerModel
serverModel =
    ()


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = messageEncoder
    , decoder = messageDecoder
    , errorWrapper = Just errorWrapper
    }


errorWrapper : Error Message -> Message
errorWrapper { description, message } =
    let
        msg =
            case message of
                Err msg ->
                    msg

                Ok msg ->
                    -- Can't happen
                    toString msg
    in
    ErrorRsp
        { kind =
            JsonDecodeError
                { messageText = description
                , decodingError = msg
                }
        , message = "Json decode error"
        }


{-| This will move into WebSocketFramework.Server
-}
otherSockets : GameId -> Socket -> WrappedModel a b c d -> List Socket
otherSockets gameid socket (WrappedModel model) =
    case Dict.get gameid model.socketsDict of
        Nothing ->
            []

        Just sockets ->
            LE.remove socket sockets


type alias Model =
    WebSocketFramework.Server.Model ServerModel Message GameState Player


{-| This will move into WebSocketFramework.Server
-}
updateGameAndPlayerids : WrappedModel a b c d -> Socket -> GameId -> PlayerId -> ( WrappedModel a b c d, GameId, PlayerId )
updateGameAndPlayerids (WrappedModel model) socket gameid playerid =
    let
        ( gid, WrappedModel mdl2 ) =
            newGameid (WrappedModel model)

        ( pid, WrappedModel mdl3 ) =
            newPlayerid (WrappedModel mdl2)

        state =
            mdl3.state

        gameDict =
            case Dict.get gameid state.gameDict of
                Nothing ->
                    state.gameDict

                Just gamestate ->
                    Dict.insert gid gamestate <|
                        Dict.remove gameid state.gameDict

        playerDict =
            case Dict.get playerid state.playerDict of
                Nothing ->
                    state.playerDict

                Just info ->
                    Dict.insert pid { info | gameid = gid } <|
                        Dict.remove playerid state.playerDict

        mdl4 =
            { mdl3
                | gameidDict =
                    Dict.insert socket gid mdl3.gameidDict
                , socketsDict =
                    Dict.insert gid [ socket ] mdl2.socketsDict
                , playeridDict =
                    Dict.insert gid [ pid ] mdl2.playeridDict
                , state =
                    { state
                        | gameDict = gameDict
                        , playerDict = playerDict
                    }
            }
    in
    ( WrappedModel mdl4, gid, pid )


{-| This will move into WebSocketFramework.Server
-}
updatePlayerid : WrappedModel a b c d -> Socket -> GameId -> PlayerId -> ( WrappedModel a b c d, GameId, PlayerId )
updatePlayerid (WrappedModel model) socket gameid playerid =
    let
        ( pid, WrappedModel mdl2 ) =
            newPlayerid (WrappedModel model)

        sockets =
            case Dict.get gameid mdl2.socketsDict of
                Nothing ->
                    [ socket ]

                Just socks ->
                    if List.member socket socks then
                        socks
                    else
                        socket :: socks

        playerids =
            case Dict.get gameid mdl2.playeridDict of
                Nothing ->
                    [ pid ]

                Just pids ->
                    pid :: pids

        state =
            mdl2.state

        playerDict =
            case Dict.get playerid state.playerDict of
                Nothing ->
                    state.playerDict

                Just info ->
                    Dict.insert pid info <|
                        Dict.remove playerid state.playerDict

        mdl3 =
            { mdl2
                | gameidDict =
                    Dict.insert socket gameid mdl2.gameidDict
                , socketsDict =
                    Dict.insert gameid sockets mdl2.socketsDict
                , playeridDict =
                    Dict.insert gameid playerids mdl2.playeridDict
                , state =
                    { state
                        | playerDict = playerDict
                    }
            }
    in
    ( WrappedModel mdl3, gameid, pid )


{-| This will move into WebSocketFramework.Server
-}
removeGame : WrappedModel a b c d -> GameId -> WrappedModel a b c d
removeGame (WrappedModel model) gameid =
    let
        mdl2 =
            case Dict.get gameid model.socketsDict of
                Nothing ->
                    model

                Just sockets ->
                    { model
                        | gameidDict =
                            List.foldl Dict.remove model.gameidDict sockets
                        , socketsDict =
                            Dict.remove gameid model.socketsDict
                    }
    in
    WrappedModel
        { mdl2
            | playeridDict = Dict.remove gameid mdl2.playeridDict
        }


{-| This will move into WebSocketFramework.Server

-- Probably want a version that doesn't remove (some) public games.

-}
removePlayerid : WrappedModel a b c d -> GameId -> PlayerId -> Bool -> WrappedModel a b c d
removePlayerid (WrappedModel model) gameid playerid keepGame =
    case Dict.get gameid model.playeridDict of
        Nothing ->
            WrappedModel model

        Just playerids ->
            case LE.remove playerid playerids of
                [] ->
                    if keepGame then
                        WrappedModel
                            { model
                                | playeridDict =
                                    Dict.remove gameid model.playeridDict
                            }
                    else
                        removeGame (WrappedModel model) gameid

                playerids ->
                    WrappedModel
                        { model
                            | playeridDict =
                                Dict.insert gameid
                                    (LE.remove playerid playerids)
                                    model.playeridDict
                        }


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender model socket state request response =
    case response of
        ReceiveRsp { chatid } ->
            model
                ! [ sendToMany (verbose model)
                        messageEncoder
                        response
                        outputPort
                    <|
                        socket
                            :: otherSockets chatid socket model
                  ]

        JoinChatRsp joinrsp ->
            let
                { chatid, memberid, memberName } =
                    joinrsp
            in
            case memberid of
                Nothing ->
                    -- Can't happen
                    model ! []

                Just mid ->
                    let
                        ( mdl, cid, mid2 ) =
                            case request of
                                NewChatReq _ ->
                                    updateGameAndPlayerids model socket chatid mid

                                _ ->
                                    updatePlayerid model socket chatid mid
                    in
                    mdl
                        ! [ sendToOne
                                (verbose mdl)
                                messageEncoder
                                (JoinChatRsp
                                    { joinrsp
                                        | chatid = cid
                                        , memberid = Just mid2
                                    }
                                )
                                outputPort
                                socket
                          , case otherSockets cid socket model of
                                [] ->
                                    Cmd.none

                                sockets ->
                                    sendToMany (verbose mdl)
                                        messageEncoder
                                        (JoinChatRsp
                                            { joinrsp
                                                | chatid = cid
                                                , memberid = Nothing
                                            }
                                        )
                                        outputPort
                                        sockets
                          ]

        LeaveChatRsp { chatid, memberName } ->
            -- Need to remove request.memberid from server-side tables.
            -- If this is the last member to leave, remove chatid
            -- from all the tables (including public chat table).
            -- Send the unchanged response to all members.
            case request of
                LeaveChatReq { memberid } ->
                    -- Change False to True if preserving a public game
                    removePlayerid model chatid memberid False
                        ! [ sendToMany (verbose model)
                                messageEncoder
                                response
                                outputPort
                            <|
                                socket
                                    :: otherSockets chatid socket model
                          ]

                _ ->
                    -- Can't happpen
                    model ! []

        _ ->
            model
                ! [ sendToOne (verbose model)
                        messageEncoder
                        response
                        outputPort
                        socket
                  ]


messageToGameid : Message -> Maybe GameId
messageToGameid message =
    case message of
        JoinChatReq { chatid } ->
            Just chatid

        JoinChatRsp { chatid } ->
            Just chatid

        ReceiveRsp { chatid } ->
            Just chatid

        LeaveChatRsp { chatid } ->
            Just chatid

        _ ->
            Nothing


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = messageProcessor
    , messageSender = messageSender
    , messageToGameid = Just messageToGameid
    , inputPort = inputPort
    , outputPort = outputPort
    }


main =
    WebSocketFramework.Server.program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
