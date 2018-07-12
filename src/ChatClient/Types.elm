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
        ( ChatKey
        , ErrorKind(..)
        , GameState
        , MemberName
        , MemberNames
        , Message(..)
        , Player
        , PublicChat
        , PublicChatName
        , RejoinMethod(..)
        , SavedModel
        , WhichPage(..)
        )

import Debug exposing (log)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , ServerInterface
        , ServerUrl
        )


type alias GameState =
    { members : List ( PlayerId, Player )
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


type alias ChatKey =
    ( ServerUrl, GameId )


type WhichPage
    = MainPage
    | PublicChatsPage


{-| Subset of ChatClient.Model fields that need to be persistent
-}
type alias SavedModel =
    { whichPage : WhichPage
    , chatKeys : List ChatKey --the ChatInfo are saved separately
    , currentChat : ChatKey
    , memberName : String
    , serverUrl : String
    , isRemote : Bool
    , chatName : String
    , chatid : String
    , publicChatName : String
    , hideHelp : Bool
    , showNotifications : Bool
    }


type ErrorKind
    = JsonDecodeError { messageText : String, decodingError : String }
    | PublicChatNameExistsError { chatName : PublicChatName }
    | UnknownChatidError { chatid : GameId }
    | MemberExistsError { chatid : GameId, memberName : MemberName }
    | UnknownMemberidError { memberid : PlayerId }
    | TooManyGamesError
    | TooManyPublicGamesError
    | InvalidPublicGameNameError
    | UnknownRequestError { request : String }


type RejoinMethod
    = RejoinNeverLeft
    | RejoinExisting
    | RejoinNew


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
    | RejoinChatReq
        { memberid : PlayerId
        , chatid : GameId
        , memberName : MemberName
        , isPublic : Bool
        }
    | JoinChatRsp
        { chatid : GameId
        , memberid : Maybe PlayerId
        , memberName : MemberName
        , otherMembers : MemberNames
        , isPublic : Bool
        , rejoinMethod : Maybe RejoinMethod
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
