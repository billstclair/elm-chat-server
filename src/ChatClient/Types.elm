---------------------------------------------------------------------
--
-- Types.elm
-- Types for ElmChatClient example
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ChatClient.Types
    exposing
        ( ErrorKind(..)
        , GameState
        , MemberName
        , MemberNames
        , Message(..)
        , Player
        , PublicChat
        , PublicChatName
        )

import Debug exposing (log)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework.Types exposing (GameId, PlayerId)


type alias GameState =
    { members : List ( PlayerId, MemberName )
    , memberCount : Int
    , gameCount : Int
    }


type alias MemberName =
    String


type alias MemberNames =
    List MemberName


type alias Player =
    MemberName


type alias PublicChatName =
    GameId


type alias PublicChat =
    { memberName : MemberName
    , chatName : PublicChatName
    , memberCount : Int
    }


type ErrorKind
    = JsonDecodeError { messageText : String, decodingError : String }
    | PublicChatNameExistsError { chatName : PublicChatName }
    | UnknownChatidError { chatid : GameId }
    | MemberExistsError { chatid : GameId, memberName : MemberName }
    | UnknownMemberidError { memberid : PlayerId }
    | UnknownRequestError { request : String }


type Message
    = PingReq { message : String }
    | PongRsp { message : String }
    | NewChatReq { memberName : MemberName }
    | NewPublicChatReq
        { memberName : MemberName
        , chatName : PublicChatName
        }
    | JoinChatReq
        { chatid : GameId
        , memberName : MemberName
        }
    | JoinChatRsp
        { chatid : GameId
        , memberid : Maybe PlayerId
        , memberName : MemberName
        , otherMembers : MemberNames
        , isPublic : Bool
        }
    | SendReq
        { memberid : PlayerId
        , message : String
        }
    | ReceiveRsp
        { chatid : GameId
        , memberName : MemberName
        , message : String
        }
    | LeaveChatReq { memberid : PlayerId }
    | LeaveChatRsp
        { chatid : GameId
        , memberName : MemberName
        }
    | GetPublicChatsReq
    | GetPublicChatsRsp { chats : List PublicChat }
    | ErrorRsp
        { kind : ErrorKind
        , message : String
        }
