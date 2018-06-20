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
        ( GameState
        , Message(..)
        , Player
        )
import Dict
import List.Extra as LE
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , Socket
        , UserFunctions
        , WrappedModel(..)
        , sendToMany
        , sendToOne
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
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
    , errorWrapper = Nothing
    }


otherSockets : GameId -> Socket -> Model -> List Socket
otherSockets gameid socket model =
    case Dict.get gameid model.socketsDict of
        Nothing ->
            []

        Just sockets ->
            LE.remove socket sockets


type alias Model =
    WebSocketFramework.Server.Model ServerModel Message GameState Player


{-| TODO
-}
newGameAndPlayerIds : Model -> GameId -> Maybe PlayerId -> ( Model, GameId, Maybe PlayerId )
newGameAndPlayerIds model chatid memberid =
    ( model, chatid, memberid )


{-| TODO
-}
newPlayerId : Model -> GameId -> Maybe PlayerId -> ( Model, GameId, Maybe PlayerId )
newPlayerId model chatid memberid =
    ( model, chatid, memberid )


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender wrappedModel socket state request response =
    let
        (WrappedModel model) =
            wrappedModel
    in
    case response of
        JoinChatRsp joinrsp ->
            let
                { chatid, memberid, memberName } =
                    joinrsp
            in
            let
                ( mdl, newChatid, newMemberid ) =
                    case request of
                        NewChatReq _ ->
                            newGameAndPlayerIds model chatid memberid

                        _ ->
                            newPlayerId model chatid memberid
            in
            WrappedModel mdl
                ! [ sendToOne
                        messageEncoder
                        (JoinChatRsp
                            { joinrsp
                                | chatid = newChatid
                                , memberid = newMemberid
                            }
                        )
                        outputPort
                        socket
                  , sendToMany
                        messageEncoder
                        (JoinChatRsp
                            { joinrsp
                                | chatid = newChatid
                                , memberid = Nothing
                            }
                        )
                        outputPort
                    <|
                        otherSockets newChatid socket model
                  ]

        LeaveChatRsp { chatid, memberName } ->
            -- Need to remove request.memberid from server-side tables.
            -- If this is the last member to leave, remove chatid
            -- from all the tables (including public chat table).
            -- Send the unchanged response to all members.
            wrappedModel
                ! [ sendToOne messageEncoder response outputPort socket ]

        _ ->
            wrappedModel
                ! [ sendToOne messageEncoder response outputPort socket ]


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
