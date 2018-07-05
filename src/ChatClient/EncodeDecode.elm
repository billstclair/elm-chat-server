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

import ChatClient.Types
    exposing
        ( ChatKey
        , ErrorKind(..)
        , MemberName
        , Message(..)
        , PublicChat
        , SavedChatInfo
        , SavedModel
        , WhichPage(..)
        )
import Dict exposing (Dict)
import ElmChat
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (decode, required)
import Json.Encode as JE exposing (Value)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.EncodeDecode exposing (genericMessageDecoder)
import WebSocketFramework.Types
    exposing
        ( DecoderPlist
        , GameId
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

        ErrorRsp { kind, message } ->
            ( Rsp "error"
            , [ ( "kind", encodeErrorKind kind )
              , ( "message", JE.string message )
              ]
            )


encodeErrorKind : ErrorKind -> Value
encodeErrorKind kind =
    case kind of
        JsonDecodeError { messageText, decodingError } ->
            JE.object
                [ ( "kind", JE.string "JsonDecodeError" )
                , ( "messageText", JE.string messageText )
                , ( "decodingError", JE.string decodingError )
                ]

        PublicChatNameExistsError { chatName } ->
            JE.object
                [ ( "kind", JE.string "PublicChatNameExistsError" )
                , ( "chatName", JE.string chatName )
                ]

        UnknownChatidError { chatid } ->
            JE.object
                [ ( "kind", JE.string "UnknownChatidError" )
                , ( "chatid", JE.string chatid )
                ]

        MemberExistsError { chatid, memberName } ->
            JE.object
                [ ( "kind", JE.string "MemberExistsError" )
                , ( "chatid", JE.string chatid )
                , ( "memberName", JE.string memberName )
                ]

        UnknownMemberidError { memberid } ->
            JE.object
                [ ( "kind", JE.string "UnknownMemberidError" )
                , ( "memberid", JE.string memberid )
                ]

        TooManyGamesError ->
            JE.object
                [ ( "kind", JE.string "TooManyGamesError" ) ]

        TooManyPublicGamesError ->
            JE.object
                [ ( "kind", JE.string "TooManyPublicGamesError" ) ]

        InvalidPublicGameNameError ->
            JE.object
                [ ( "kind", JE.string "InvalidPublicGameNameError" ) ]

        UnknownRequestError { request } ->
            JE.object
                [ ( "kind", JE.string "UnknownRequestError" )
                , ( "request", JE.string request )
                ]


encodePublicChat : PublicChat -> Value
encodePublicChat { memberName, chatName, memberCount } =
    JE.object
        [ ( "memberName", JE.string memberName )
        , ( "chatName", JE.string chatName )
        , ( "memberCount", JE.int memberCount )
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
    JD.map3
        (\memberName chatName memberCount ->
            { memberName = memberName
            , chatName = chatName
            , memberCount = memberCount
            }
        )
        (JD.field "memberName" JD.string)
        (JD.field "chatName" JD.string)
        (JD.field "memberCount" JD.int)


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
        (\kind message ->
            ErrorRsp
                { kind = kind
                , message = message
                }
        )
        (JD.field "kind" errorKindDecoder)
        (JD.field "message" JD.string)


errorKindDecoder : Decoder ErrorKind
errorKindDecoder =
    JD.field "kind" JD.string
        |> JD.andThen errorKindStringDecoder


errorKindStringDecoder : String -> Decoder ErrorKind
errorKindStringDecoder kind =
    case kind of
        "JsonDecodeError" ->
            JD.map2
                (\messageText decodingError ->
                    JsonDecodeError
                        { messageText = messageText
                        , decodingError = decodingError
                        }
                )
                (JD.field "messageText" JD.string)
                (JD.field "decodingError" JD.string)

        "PublicChatNameExistsError" ->
            JD.map
                (\chatName ->
                    PublicChatNameExistsError
                        { chatName = chatName }
                )
                (JD.field "chatName" JD.string)

        "UnknownChatidError" ->
            JD.map
                (\chatid ->
                    UnknownChatidError { chatid = chatid }
                )
                (JD.field "chatid" JD.string)

        "MemberExistsError" ->
            JD.map2
                (\chatid memberName ->
                    MemberExistsError
                        { chatid = chatid
                        , memberName = memberName
                        }
                )
                (JD.field "chatid" JD.string)
                (JD.field "memberName" JD.string)

        "UnknownMemberidError" ->
            JD.map
                (\memberid ->
                    UnknownMemberidError { memberid = memberid }
                )
                (JD.field "memberid" JD.string)

        "TooManyGamesError" ->
            JD.succeed TooManyGamesError

        "TooManyPublicGamesError" ->
            JD.succeed TooManyPublicGamesError

        "InvalidPublicGameNameError" ->
            JD.succeed InvalidPublicGameNameError

        "UnknownRequestError" ->
            JD.map
                (\request ->
                    UnknownRequestError
                        { request = request }
                )
                (JD.field "request" JD.string)

        _ ->
            JD.fail <| "Unknown error kind: " ++ kind


{-| ChatKey needs to be turned into a string so it can be a value

for an HTML.select Html.option.

-}
encodeChatKey : ChatKey -> String
encodeChatKey chatkey =
    chatKeyEncoder chatkey
        |> JE.encode 0


chatKeyEncoder : ChatKey -> Value
chatKeyEncoder =
    stringPairEncoder


stringPairEncoder : ( String, String ) -> Value
stringPairEncoder ( s1, s2 ) =
    JE.list [ JE.string s1, JE.string s2 ]


decodeChatKey : String -> Result String ChatKey
decodeChatKey json =
    JD.decodeString chatKeyDecoder json


chatKeyDecoder : Decoder ChatKey
chatKeyDecoder =
    stringPairDecoder "Malformed ChatKey"


stringPairDecoder : String -> Decoder ( String, String )
stringPairDecoder msg =
    JD.list JD.string
        |> JD.andThen
            (\pair ->
                case pair of
                    [ s1, s2 ] ->
                        JD.succeed ( s1, s2 )

                    _ ->
                        JD.fail msg
            )


{-| Persistence for ChatClient.Model
-}
savedModelEncoder : SavedModel -> Value
savedModelEncoder model =
    JE.object
        [ ( "whichPage", whichPageEncoder model.whichPage )
        , ( "chatKeys", savedChatKeysEncoder model.chatKeys )
        , ( "currentChat", chatKeyEncoder model.currentChat )
        , ( "memberName", JE.string model.memberName )
        , ( "serverUrl", JE.string model.serverUrl )
        , ( "isRemote", JE.bool model.isRemote )
        , ( "chatName", JE.string model.chatName )
        , ( "chatid", JE.string model.chatid )
        , ( "publicChatName", JE.string model.publicChatName )
        , ( "hideHelp", JE.bool model.hideHelp )
        ]


decodeSavedModel : Value -> Result String SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    decode SavedModel
        |> required "whichPage" whichPageDecoder
        |> required "chatKeys" savedChatKeysDecoder
        |> required "currentChat" chatKeyDecoder
        |> required "memberName" JD.string
        |> required "serverUrl" JD.string
        |> required "isRemote" JD.bool
        |> required "chatName" JD.string
        |> required "chatid" JD.string
        |> required "publicChatName" JD.string
        |> required "hideHelp" JD.bool


whichPageEncoder : WhichPage -> Value
whichPageEncoder whichPage =
    case whichPage of
        MainPage ->
            JE.string "MainPage"

        PublicChatsPage ->
            JE.string "PublicChatsPage"


whichPageDecoder : Decoder WhichPage
whichPageDecoder =
    JD.string
        |> JD.andThen
            (\page ->
                case page of
                    "MainPage" ->
                        JD.succeed MainPage

                    "PublicChatsPage" ->
                        JD.succeed PublicChatsPage

                    _ ->
                        JD.fail <| "Invalid page: " ++ page
            )


savedChatKeysEncoder : List ChatKey -> Value
savedChatKeysEncoder keys =
    JE.list <|
        List.map chatKeyEncoder keys


savedChatKeysDecoder : Decoder (List ChatKey)
savedChatKeysDecoder =
    JD.list chatKeyDecoder


savedChatEncoder : SavedChatInfo msg -> Value
savedChatEncoder chat =
    JE.object
        [ ( "chatName", JE.string chat.chatName )
        , ( "members"
          , JE.list <|
                List.map stringPairEncoder chat.members
          )
        , ( "serverUrl", JE.string chat.serverUrl )
        , ( "chatid", JE.string chat.chatid )
        , ( "isPublic", JE.bool chat.isPublic )
        , ( "settings", ElmChat.settingsEncoder chat.settings )
        ]


memberDecoder : Decoder ( GameId, MemberName )
memberDecoder =
    stringPairDecoder "member (id, name)"


savedChatDecoder : ElmChat.Updater msg -> Decoder (SavedChatInfo msg)
savedChatDecoder updater =
    decode SavedChatInfo
        |> required "chatName" JD.string
        |> required "members" (JD.list memberDecoder)
        |> required "serverUrl" JD.string
        |> required "chatid" JD.string
        |> required "isPublic" JD.bool
        |> required "settings" (ElmChat.settingsDecoder updater)
