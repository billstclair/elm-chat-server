---------------------------------------------------------------------
--
-- Interface.elm
-- Processor for ChatClient example
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ChatClient.Interface
    exposing
        ( getPublicGame
        , messageProcessor
        )

import ChatClient.Types
    exposing
        ( GameState
        , MemberName
        , MemberNames
        , Message(..)
        , Player
        , PublicChatName
        )
import Debug exposing (log)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.ServerInterface exposing (dummyGameid)
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , PublicGame
        , ReqRsp(..)
        , ServerState
        )


{-| This should move into WebSocketFramework,
and publicGames should become a Dict.
-}
getPublicGame : GameId -> ServerState GameState Player -> Maybe PublicGame
getPublicGame chatid state =
    case List.filter (.gameid >> (==) chatid) state.publicGames of
        game :: _ ->
            Just game

        _ ->
            Nothing


messageProcessor : ServerState GameState Player -> Message -> ( ServerState GameState Player, Maybe Message )
messageProcessor state message =
    case message of
        NewChatReq { memberName } ->
            ( { state
                | gameDict =
                    Dict.insert dummyGameid { members = [] } state.gameDict
              }
            , Just <|
                JoinChatRsp
                    -- chatid and memberid filled in by server code
                    -- can't do it here, because we have no random seed available.
                    { chatid = dummyGameid
                    , memberid = Nothing
                    , memberName = memberName
                    , otherMembers = []
                    , isPublic = False
                    }
            )

        JoinChatReq { chatid, memberName } ->
            case Dict.get chatid state.gameDict of
                Nothing ->
                    ( state
                    , Just <|
                        ErrorRsp
                            { chatid = chatid
                            , message = "No such chatid."
                            }
                    )

                Just gameState ->
                    if List.member memberName gameState.members then
                        ( state
                        , Just <|
                            ErrorRsp
                                { chatid = chatid
                                , message = "Already a member with that name in this chat."
                                }
                        )
                    else
                        ( { state
                            | gameDict =
                                Dict.insert chatid
                                    { gameState
                                        | members = memberName :: gameState.members
                                    }
                                    state.gameDict
                          }
                        , Just <|
                            JoinChatRsp
                                { chatid = chatid
                                , memberid = Nothing
                                , memberName = memberName
                                , otherMembers = gameState.members
                                , isPublic = Nothing /= getPublicGame chatid state
                                }
                        )

        SendReq { memberid, message } ->
            ( state
            , Just <|
                case Dict.get memberid state.playerDict of
                    Nothing ->
                        ErrorRsp
                            { chatid = memberid
                            , message = "Unknown memberid."
                            }

                    Just info ->
                        ReceiveRsp
                            { chatid = info.gameid
                            , memberName = info.player
                            , message = message
                            }
            )

        LeaveChatReq { memberid } ->
            case Dict.get memberid state.playerDict of
                Nothing ->
                    ( state
                    , Just <|
                        ErrorRsp
                            { chatid = memberid
                            , message = "Unknown memberid"
                            }
                    )

                Just info ->
                    case Dict.get info.gameid state.gameDict of
                        Nothing ->
                            ( state
                            , Just <|
                                ErrorRsp
                                    { chatid = info.gameid
                                    , message = "Can't find gameid. Shouldn't happen."
                                    }
                            )

                        Just gameState ->
                            let
                                members =
                                    LE.remove info.player gameState.members

                                state2 =
                                    if members == [] then
                                        { state
                                            | gameDict =
                                                Dict.remove
                                                    info.gameid
                                                    state.gameDict
                                        }
                                    else
                                        { state
                                            | gameDict =
                                                Dict.insert
                                                    info.gameid
                                                    { gameState
                                                        | members = members
                                                    }
                                                    state.gameDict
                                        }
                            in
                            ( state2
                            , Just <|
                                LeaveChatRsp
                                    { chatid = info.gameid
                                    , memberName = info.player
                                    }
                            )

        _ ->
            -- They must have sent a response. Not valid.
            ( state
            , Just <|
                ErrorRsp
                    { chatid = ""
                    , message = "Unknown request: " ++ toString message
                    }
            )
