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


dummyMemberid : PlayerId
dummyMemberid =
    "<memberid>"


{-| This is a ServerMessageProcessor GameState Player Message
-}
messageProcessor : ServerState GameState Player -> Message -> ( ServerState GameState Player, Maybe Message )
messageProcessor state message =
    case message of
        PingReq { message } ->
            ( state, Just <| PongRsp { message = message } )

        NewChatReq { memberName } ->
            let
                serverState =
                    case state.state of
                        Nothing ->
                            { members = []
                            , memberCount = 0
                            , gameCount = 0
                            }

                        Just ss ->
                            ss

                gameCount =
                    serverState.gameCount + 1

                gameid =
                    toString gameCount

                playerid =
                    "P1"

                state2 =
                    { state
                        | state =
                            Just { serverState | gameCount = gameCount }
                    }
            in
            ( { state2
                | gameDict =
                    Dict.insert
                        gameid
                        { members = [ memberName ]
                        , memberCount = 1
                        , gameCount = 0 -- This isn't used for individual games.
                        }
                        state.gameDict
                , playerDict =
                    Dict.insert
                        playerid
                        { gameid = gameid
                        , player = memberName
                        }
                        state.playerDict
              }
            , Just <|
                JoinChatRsp
                    -- chatid and memberid filled in by server code
                    -- can't do it here, because we have no random seed available.
                    { chatid = gameid
                    , memberid = Just playerid
                    , memberName = memberName
                    , otherMembers = []
                    , isPublic = False
                    }
            )

        NewPublicChatReq { memberName, chatName } ->
            let
                playerid =
                    "P1"
            in
            case Dict.get chatName state.gameDict of
                Just _ ->
                    ( state
                    , Just <|
                        ErrorRsp
                            { chatid = chatName
                            , message = "There is already a game with that name."
                            }
                    )

                Nothing ->
                    ( { state
                        | gameDict =
                            Dict.insert
                                chatName
                                { members = [ memberName ]
                                , memberCount = 1
                                , gameCount = 0
                                }
                                state.gameDict
                        , publicGames =
                            { gameid = chatName
                            , playerName = memberName
                            }
                                :: state.publicGames
                        , playerDict =
                            Dict.insert
                                playerid
                                { gameid = chatName
                                , player = memberName
                                }
                                state.playerDict
                      }
                    , Just <|
                        JoinChatRsp
                            { chatid = chatName
                            , memberid = Just playerid
                            , memberName = memberName
                            , otherMembers = []
                            , isPublic = True
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
                        let
                            memberCount =
                                gameState.memberCount + 1

                            memberid =
                                "P" ++ toString memberCount

                            gs2 =
                                { gameState
                                    | memberCount = memberCount
                                }
                        in
                        ( { state
                            | gameDict =
                                Dict.insert chatid
                                    { gs2
                                        | members = memberName :: gameState.members
                                    }
                                    state.gameDict
                            , playerDict =
                                Dict.insert
                                    memberid
                                    { gameid = chatid
                                    , player = memberName
                                    }
                                    state.playerDict
                          }
                        , Just <|
                            JoinChatRsp
                                { chatid = chatid
                                , memberid = Just memberid
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
                                    { state
                                        | playerDict =
                                            Dict.remove memberid state.playerDict
                                    }

                                state3 =
                                    if members == [] then
                                        { state2
                                            | gameDict =
                                                Dict.remove
                                                    info.gameid
                                                    state.gameDict
                                        }
                                    else
                                        { state2
                                            | gameDict =
                                                Dict.insert
                                                    info.gameid
                                                    { gameState
                                                        | members = members
                                                    }
                                                    state.gameDict
                                        }
                            in
                            ( state3
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