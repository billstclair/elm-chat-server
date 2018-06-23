---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoding and decoding for ElmChatClient example
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ChatClient.EncodeDecode exposing (..)

import ChatClient.Types exposing (Message(..), PublicChat)
import Dict
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.EncodeDecode exposing (genericMessageDecoder)
import WebSocketFramework.Types
    exposing
        ( DecoderPlist
        , MessageDecoder
        , MessageEncoder
        , Plist
        , PublicGame
        , ReqRsp(..)
        )


messageEncoder : MessageEncoder Message
messageEncoder message =
    case message of
        PingReq { message } ->
            ( Req "ping"
            , [ ( "message", JE.string message ) ]
            )

        PongRsp { message } ->
            ( Rsp "pong"
            , [ ( "message", JE.string message ) ]
            )

        NewChatReq { memberName } ->
            ( Req "new"
            , [ ( "memberName", JE.string memberName ) ]
            )

        NewPublicChatReq { memberName, chatName } ->
            ( Req "newPublic"
            , [ ( "memberName", JE.string memberName )
              , ( "chatName", JE.string chatName )
              ]
            )

        JoinChatReq { chatid, memberName } ->
            ( Req "join"
            , [ ( "chatid", JE.string chatid )
              , ( "memberName", JE.string memberName )
              ]
            )

        JoinChatRsp { chatid, memberid, memberName, otherMembers, isPublic } ->
            ( Rsp "join"
            , [ ( "chatid", JE.string chatid )
              , ( "memberid"
                , case memberid of
                    Nothing ->
                        JE.null

                    Just id ->
                        JE.string id
                )
              , ( "memberName", JE.string memberName )
              , ( "otherMembers"
                , JE.list <| List.map JE.string otherMembers
                )
              , ( "isPublic", JE.bool isPublic )
              ]
            )

        SendReq { memberid, message } ->
            ( Req "send"
            , [ ( "memberid", JE.string memberid )
              , ( "message", JE.string message )
              ]
            )

        ReceiveRsp { chatid, memberName, message } ->
            ( Rsp "receive"
            , [ ( "chatid", JE.string chatid )
              , ( "memberName", JE.string memberName )
              , ( "message", JE.string message )
              ]
            )

        LeaveChatReq { memberid } ->
            ( Req "leave"
            , [ ( "memberid", JE.string memberid ) ]
            )

        LeaveChatRsp { chatid, memberName } ->
            ( Rsp "leave"
            , [ ( "chatid", JE.string chatid )
              , ( "memberName", JE.string memberName )
              ]
            )

        GetPublicChatsReq ->
            ( Req "getPublicChats"
            , []
            )

        GetPublicChatsRsp { chats } ->
            ( Rsp "getPublicChats"
            , [ ( "chats", JE.list <| List.map encodePublicChat chats ) ]
            )

        ErrorRsp { chatid, message } ->
            ( Rsp "error"
            , [ ( "chatid", JE.string chatid )
              , ( "message", JE.string message )
              ]
            )


encodePublicChat : PublicChat -> Value
encodePublicChat { memberName, chatName } =
    JE.object
        [ ( "memberName", JE.string memberName )
        , ( "chatName", JE.string chatName )
        ]


messageDecoder : MessageDecoder Message
messageDecoder reqrspAndPlist =
    genericMessageDecoder reqPlist rspPlist reqrspAndPlist


reqPlist : DecoderPlist Message
reqPlist =
    [ ( "ping", pingReqDecoder )
    , ( "new", newChatReqDecoder )
    , ( "newPublic", newPublicChatReqDecoder )
    , ( "join", joinChatReqDecoder )
    , ( "send", sendReqDecoder )
    , ( "leave", leaveChatReqDecoder )
    , ( "getPublicChats", getPublicChatsReqDecoder )
    ]


rspPlist : DecoderPlist Message
rspPlist =
    [ ( "pong", pongRspDecoder )
    , ( "join", joinChatRspDecoder )
    , ( "receive", receiveRspDecoder )
    , ( "leave", leaveChatRspDecoder )
    , ( "getPublicChats", getPublicChatsRspDecoder )
    , ( "error", errorRspDecoder )
    ]


pingReqDecoder : Decoder Message
pingReqDecoder =
    JD.map (\message -> PingReq { message = message })
        (JD.field "message" JD.string)


newChatReqDecoder : Decoder Message
newChatReqDecoder =
    JD.map (\memberName -> NewChatReq { memberName = memberName })
        (JD.field "memberName" JD.string)


newPublicChatReqDecoder : Decoder Message
newPublicChatReqDecoder =
    JD.map2
        (\memberName chatName ->
            NewPublicChatReq
                { memberName = memberName
                , chatName = chatName
                }
        )
        (JD.field "memberName" JD.string)
        (JD.field "chatName" JD.string)


joinChatReqDecoder : Decoder Message
joinChatReqDecoder =
    JD.map2
        (\chatid memberName ->
            JoinChatReq
                { chatid = chatid
                , memberName = memberName
                }
        )
        (JD.field "chatid" JD.string)
        (JD.field "memberName" JD.string)


sendReqDecoder : Decoder Message
sendReqDecoder =
    JD.map2
        (\memberid message ->
            SendReq
                { memberid = memberid
                , message = message
                }
        )
        (JD.field "memberid" JD.string)
        (JD.field "message" JD.string)


leaveChatReqDecoder : Decoder Message
leaveChatReqDecoder =
    JD.map
        (\memberid -> LeaveChatReq { memberid = memberid })
        (JD.field "memberid" JD.string)


pongRspDecoder : Decoder Message
pongRspDecoder =
    JD.map (\message -> PongRsp { message = message })
        (JD.field "message" JD.string)


joinChatRspDecoder : Decoder Message
joinChatRspDecoder =
    JD.map5
        (\chatid memberid memberName otherMembers isPublic ->
            JoinChatRsp
                { chatid = chatid
                , memberid = memberid
                , memberName = memberName
                , otherMembers = otherMembers
                , isPublic = isPublic
                }
        )
        (JD.field "chatid" JD.string)
        (JD.field "memberid" <| JD.nullable JD.string)
        (JD.field "memberName" JD.string)
        (JD.field "otherMembers" <| JD.list JD.string)
        (JD.field "isPublic" JD.bool)


receiveRspDecoder : Decoder Message
receiveRspDecoder =
    JD.map3
        (\chatid memberName message ->
            ReceiveRsp
                { chatid = chatid
                , memberName = memberName
                , message = message
                }
        )
        (JD.field "chatid" JD.string)
        (JD.field "memberName" JD.string)
        (JD.field "message" JD.string)


leaveChatRspDecoder : Decoder Message
leaveChatRspDecoder =
    JD.map2
        (\chatid memberName ->
            LeaveChatRsp
                { chatid = chatid
                , memberName = memberName
                }
        )
        (JD.field "chatid" JD.string)
        (JD.field "memberName" JD.string)


publicChatDecoder : Decoder PublicChat
publicChatDecoder =
    JD.map2
        (\memberName chatName ->
            { memberName = memberName
            , chatName = chatName
            }
        )
        (JD.field "memberName" JD.string)
        (JD.field "chatName" JD.string)


getPublicChatsReqDecoder : Decoder Message
getPublicChatsReqDecoder =
    JD.succeed GetPublicChatsReq


getPublicChatsRspDecoder : Decoder Message
getPublicChatsRspDecoder =
    JD.map (\chats -> GetPublicChatsRsp { chats = chats }) <|
        (JD.field "chats" <| JD.list publicChatDecoder)


errorRspDecoder : Decoder Message
errorRspDecoder =
    JD.map2
        (\chatid message ->
            ErrorRsp
                { chatid = chatid
                , message = message
                }
        )
        (JD.field "chatid" JD.string)
        (JD.field "message" JD.string)
