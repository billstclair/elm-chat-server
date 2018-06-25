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
import Dict exposing (Dict)
import List.Extra as LE
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , ServerPlayersDeleter
        , Socket
        , UserFunctions
        , WrappedModel(..)
        , newGameid
        , newPlayerid
        , otherSockets
        , removePlayer
        , sendToMany
        , sendToOne
        , updateGameAndPlayerids
        , updatePlayerid
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


type alias Model =
    WebSocketFramework.Server.Model ServerModel Message GameState Player


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

                Just oldmid ->
                    let
                        ( mdl, cid, mid ) =
                            case request of
                                NewChatReq _ ->
                                    updateGameAndPlayerids model socket chatid oldmid

                                _ ->
                                    updatePlayerid model socket chatid oldmid
                    in
                    let
                        (WrappedModel m1) =
                            mdl

                        m =
                            { m1
                                | state =
                                    replaceMemberid cid oldmid mid m1.state
                            }
                    in
                    -- deathRowDuration change is temporary
                    WrappedModel m
                        ! [ sendToOne
                                (verbose mdl)
                                messageEncoder
                                (JoinChatRsp
                                    { joinrsp
                                        | chatid = cid
                                        , memberid = Just mid
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
                    removePlayer model chatid memberid False
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


replaceMemberid : GameId -> PlayerId -> PlayerId -> ServerState GameState player -> ServerState GameState player
replaceMemberid gameid oldmid mid state =
    case Dict.get gameid state.gameDict of
        Nothing ->
            state

        Just gamestate ->
            let
                members =
                    List.map
                        (\pair ->
                            let
                                ( id, name ) =
                                    pair
                            in
                            if id == oldmid then
                                ( mid, name )
                            else
                                pair
                        )
                        gamestate.members
            in
            { state
                | gameDict =
                    Dict.insert gameid
                        { gamestate | members = members }
                        state.gameDict
            }


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


messageToPlayerid : Message -> Maybe PlayerId
messageToPlayerid message =
    case message of
        JoinChatRsp { memberid } ->
            case memberid of
                Nothing ->
                    Nothing

                Just mid ->
                    Just mid

        SendReq { memberid } ->
            Just memberid

        LeaveChatReq { memberid } ->
            Just memberid

        _ ->
            Nothing


{-| This will move into WebSocketFramework.Server
-}
isPublicGame : GameId -> ServerState gamestate player -> Bool
isPublicGame chatid state =
    case LE.find (.gameid >> (==) chatid) state.publicGames of
        Just _ ->
            True

        Nothing ->
            False


{-| This will move into WebSocketFramework.Server
-}
isPrivateGame : GameId -> ServerState gamestate player -> Bool
isPrivateGame gameid state =
    not <| isPublicGame gameid state


type alias ChatServerState =
    ServerState GameState Player


deletePlayer : PlayerId -> ChatServerState -> ChatServerState
deletePlayer id state =
    case Dict.get id state.playerDict of
        Nothing ->
            state

        Just info ->
            let
                state2 =
                    { state
                        | playerDict =
                            Dict.remove id state.playerDict
                    }
            in
            case Dict.get info.gameid state2.gameDict of
                Nothing ->
                    state2

                Just gamestate ->
                    let
                        members =
                            List.filter
                                (Tuple.first >> (/=) id)
                                gamestate.members
                    in
                    { state2
                        | gameDict =
                            -- Don't need to test for members == [],
                            -- because that already happened
                            -- in the autoDeleteGame call.
                            Dict.insert
                                info.gameid
                                { gamestate
                                    | members = members
                                }
                                state2.gameDict
                    }


deletePlayers : ServerPlayersDeleter ServerModel Message GameState Player
deletePlayers (WrappedModel model) playerids serverState =
    WrappedModel
        { model
            | state =
                List.foldl deletePlayer serverState playerids
        }


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = messageProcessor
    , messageSender = messageSender
    , messageToGameid = Just messageToGameid
    , messageToPlayerid = Just messageToPlayerid
    , autoDeleteGame = Just isPrivateGame
    , gamesDeleter = Nothing
    , playersDeleter = Just deletePlayers
    , inputPort = inputPort
    , outputPort = outputPort
    }


main =
    WebSocketFramework.Server.program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
