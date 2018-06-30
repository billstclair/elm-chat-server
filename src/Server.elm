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
import ChatClient.Settings exposing (settings)
import ChatClient.Types
    exposing
        ( ErrorKind(..)
        , GameState
        , Message(..)
        , Player
        )
import Debug exposing (log)
import List.Extra as LE
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , ServerPlayersDeleter
        , Socket
        , UserFunctions
        , WrappedModel(..)
        , sendToAll
        , sendToOne
        , sendToOthers
        , verbose
        )
import WebSocketFramework.ServerInterface
    exposing
        ( getGame
        , getPlayer
        , updateGame
        , updatePlayer
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
                ! [ sendToAll chatid
                        model
                        messageEncoder
                        response
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
                        (WrappedModel m1) =
                            model

                        m =
                            -- deathRowDuration change is temporary
                            { m1 | deathRowDuration = settings.deathRowDuration }
                    in
                    WrappedModel m
                        ! [ sendToOne
                                (verbose model)
                                messageEncoder
                                response
                                outputPort
                                socket
                          , sendToOthers chatid
                                socket
                                model
                                messageEncoder
                            <|
                                JoinChatRsp
                                    { joinrsp
                                        | memberid = Nothing
                                    }
                          ]

        LeaveChatRsp { chatid } ->
            model
                ! [ sendToOne (verbose model)
                        messageEncoder
                        response
                        outputPort
                        socket
                  , sendToOthers chatid
                        socket
                        model
                        messageEncoder
                        response
                  ]

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
    case getPlayer id state of
        Nothing ->
            state

        Just info ->
            let
                state2 =
                    updatePlayer id Nothing state
            in
            case getGame info.gameid state2 of
                Nothing ->
                    state2

                Just gamestate ->
                    let
                        members =
                            List.filter
                                (Tuple.first >> (/=) id)
                                gamestate.members
                    in
                    updateGame info.gameid
                        (Just { gamestate | members = members })
                        state2


deletePlayers : ServerPlayersDeleter ServerModel Message GameState Player
deletePlayers model playerids serverState =
    let
        delete =
            \playerid ( state, cmd ) ->
                case getPlayer playerid state of
                    Nothing ->
                        ( state, cmd )

                    Just info ->
                        deletePlayer playerid state
                            ! [ cmd
                              , sendToAll info.gameid
                                    model
                                    messageEncoder
                                <|
                                    LeaveChatRsp
                                        { chatid = info.gameid
                                        , memberName = info.player
                                        }
                              ]

        (WrappedModel mdl) =
            model
    in
    let
        ( state, cmd ) =
            List.foldl delete ( serverState, Cmd.none ) playerids
    in
    WrappedModel { mdl | state = state }
        ! [ cmd ]


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = messageProcessor
    , messageSender = messageSender
    , messageToGameid = Just messageToGameid
    , messageToPlayerid = Just messageToPlayerid
    , autoDeleteGame = Nothing --Just isPrivateGame
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
