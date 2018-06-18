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
        ( GameState
        , MemberName
        , MemberNames
        , Message(..)
        , Player
        , PublicChatName
        )

import Debug exposing (log)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework.Types exposing (GameId, PlayerId)


type alias GameState =
    { members : List MemberName }


type alias MemberName =
    String


type alias MemberNames =
    List MemberName


type alias Player =
    MemberName


type alias PublicChatName =
    GameId


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
    | ErrorRsp
        { chatid : GameId
        , message : String
        }
