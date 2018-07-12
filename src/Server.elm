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
import ChatClient.Interface
    exposing
        ( messageProcessor
        , messageToGameid
        , messageToPlayerid
        )
import ChatClient.Settings exposing (settings)
import ChatClient.Types
    exposing
        ( ErrorKind(..)
        , GameState
        , Message(..)
        , Player
        , RejoinMethod(..)
        )
import Debug exposing (log)
import List.Extra as LE
import WebSocketFramework.Server
    exposing
        ( ServerMessageSender
        , ServerPlayersDeleter
        , Socket
        , UserFunctions
        , WrappedModel
        , sendToAll
        , sendToOne
        , sendToOthers
        , setDeathRowDuration
        , setState
        , verbose
        )
import WebSocketFramework.ServerInterface
    exposing
        ( getGame
        , getPlayer
        , removePlayer
        , updateGame
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


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender model socket state request response =
    case response of
        ReceiveRsp { chatid, message } ->
            model
                ! [ if message == "" then
                        -- Empty message is to test for existence of memberid.
                        -- Send it back to only the requester.
                        sendToOne (verbose model)
                            messageEncoder
                            response
                            outputPort
                            socket
                    else
                        sendToAll chatid
                            model
                            messageEncoder
                            response
                  ]

        -- Need to special case RejoinChatReq here
        -- Don't send the response to anybody else.
        JoinChatRsp joinrsp ->
            let
                { chatid, memberid, memberName, rejoinMethod } =
                    joinrsp
            in
            case memberid of
                Nothing ->
                    -- Can't happen
                    model ! []

                Just oldmid ->
                    let
                        mdl =
                            setDeathRowDuration model settings.deathRowDuration
                    in
                    mdl
                        ! [ sendToOne
                                (verbose model)
                                messageEncoder
                                response
                                outputPort
                                socket
                          , case rejoinMethod of
                                Just RejoinNeverLeft ->
                                    Cmd.none

                                _ ->
                                    sendToOthers chatid
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
                    removePlayer id state
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
                        { gamestate | members = members }
                        state2


deletePlayers : ServerPlayersDeleter ServerModel Message GameState Player
deletePlayers model chatid memberids serverState =
    case getGame chatid serverState of
        Nothing ->
            model ! []

        Just gameState ->
            let
                delete =
                    \memberid ( gs, cmd ) ->
                        case getPlayer memberid serverState of
                            Nothing ->
                                ( gs, cmd )

                            Just info ->
                                { gs
                                    | members =
                                        List.filter (\( mid, _ ) -> mid /= memberid)
                                            gs.members
                                }
                                    ! [ cmd
                                      , sendToAll chatid
                                            model
                                            messageEncoder
                                        <|
                                            LeaveChatRsp
                                                { chatid = chatid
                                                , memberName = info.player
                                                }
                                      ]

                ( gs2, cmd ) =
                    List.foldl delete ( gameState, Cmd.none ) memberids
            in
            setState model (updateGame chatid gs2 serverState)
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
