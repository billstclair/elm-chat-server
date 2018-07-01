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

import ChatClient.Settings exposing (settings)
import ChatClient.Types
    exposing
        ( ErrorKind(..)
        , GameState
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
import WebSocketFramework.ServerInterface as ServerInterface
    exposing
        ( gameCount
        , getGame
        , getPlayer
        , updateGame
        , updatePlayer
        )
import WebSocketFramework.Types as Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , PublicGame
        , ReqRsp(..)
        )


type alias ServerState =
    Types.ServerState GameState Player


{-| This should move into WebSocketFramework,
and publicGames should become a Dict.
-}
getPublicGame : GameId -> ServerState -> Maybe PublicGame
getPublicGame gameid state =
    case List.filter (.gameid >> (==) gameid) state.publicGames of
        game :: _ ->
            Just game

        _ ->
            Nothing


isPublicGame : GameId -> ServerState -> Bool
isPublicGame gameid state =
    case getPublicGame gameid state of
        Nothing ->
            False

        Just _ ->
            True


dummyMemberid : PlayerId
dummyMemberid =
    "<memberid>"


{-| This is a ServerMessageProcessor GameState Player Message
-}
messageProcessor : ServerState -> Message -> ( ServerState, Maybe Message )
messageProcessor state message =
    case message of
        PingReq { message } ->
            ( state, Just <| PongRsp { message = message } )

        NewChatReq { memberName } ->
            newChatReq state memberName

        NewPublicChatReq { memberName, chatName } ->
            newPublicChatReq state memberName chatName

        JoinChatReq { chatid, memberName } ->
            case getGame chatid state of
                Nothing ->
                    ( state
                    , Just <|
                        ErrorRsp
                            { kind = UnknownChatidError { chatid = chatid }
                            , message = "No such chatid."
                            }
                    )

                Just gamestate ->
                    if
                        Nothing
                            /= LE.find (Tuple.second >> (==) memberName)
                                gamestate.members
                    then
                        ( state
                        , Just <|
                            ErrorRsp
                                { kind =
                                    MemberExistsError
                                        { chatid = chatid
                                        , memberName = memberName
                                        }
                                , message =
                                    "There is already a member with that name in this chat."
                                }
                        )
                    else
                        let
                            ( memberid, state2 ) =
                                ServerInterface.newPlayerid state

                            state3 =
                                updateGame chatid
                                    (Just
                                        { gamestate
                                            | members =
                                                ( memberid, memberName )
                                                    :: gamestate.members
                                        }
                                    )
                                    state2

                            info =
                                { gameid = chatid
                                , player = memberName
                                }
                        in
                        ( ServerInterface.addPlayer memberid info state3
                        , Just <|
                            JoinChatRsp
                                { chatid = chatid
                                , memberid = Just memberid
                                , memberName = memberName
                                , otherMembers =
                                    List.map Tuple.second
                                        gamestate.members
                                , isPublic = Nothing /= getPublicGame chatid state
                                }
                        )

        SendReq { memberid, message } ->
            ( state
            , Just <|
                case getPlayer memberid state of
                    Nothing ->
                        ErrorRsp
                            { kind = UnknownMemberidError { memberid = memberid }
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
            case getPlayer memberid state of
                Nothing ->
                    ( state
                    , Just <|
                        ErrorRsp
                            { kind =
                                UnknownMemberidError
                                    { memberid = memberid }
                            , message = "Unknown memberid"
                            }
                    )

                Just info ->
                    case getGame info.gameid state of
                        Nothing ->
                            ( state
                            , Just <|
                                ErrorRsp
                                    { kind =
                                        UnknownChatidError
                                            { chatid = info.gameid }
                                    , message =
                                        "Can't find chatid. Shouldn't happen."
                                    }
                            )

                        Just gamestate ->
                            let
                                members =
                                    List.filter (Tuple.first >> (/=) memberid)
                                        gamestate.members

                                state2 =
                                    if members == [] then
                                        ServerInterface.removeGame
                                            info.gameid
                                            (List.map Tuple.first
                                                gamestate.members
                                            )
                                            state
                                    else
                                        ServerInterface.removePlayer memberid <|
                                            updateGame info.gameid
                                                (Just
                                                    { gamestate
                                                        | members = members
                                                    }
                                                )
                                                state
                            in
                            ( state2
                            , Just <|
                                LeaveChatRsp
                                    { chatid = info.gameid
                                    , memberName = info.player
                                    }
                            )

        GetPublicChatsReq ->
            ( state
            , Just <|
                GetPublicChatsRsp
                    { chats =
                        List.map
                            (\game ->
                                { memberName = game.playerName
                                , chatName = game.gameid
                                , memberCount = gameMemberCount state game.gameid
                                }
                            )
                            state.publicGames
                    }
            )

        _ ->
            -- They must have sent a response. Not valid.
            ( state
            , Just <|
                ErrorRsp
                    { kind =
                        UnknownRequestError
                            { request = toString message }
                    , message = "Unknown request"
                    }
            )


gameMemberCount : ServerState -> GameId -> Int
gameMemberCount state gameid =
    case getGame gameid state of
        Nothing ->
            0

        Just gamestate ->
            List.length gamestate.members


newPublicChatReq : ServerState -> MemberName -> GameId -> ( ServerState, Maybe Message )
newPublicChatReq state memberName chatName =
    case getGame chatName state of
        Just _ ->
            ( state
            , Just <|
                ErrorRsp
                    { kind =
                        PublicChatNameExistsError
                            { chatName = chatName }
                    , message =
                        "There is already a public chat with that name."
                    }
            )

        Nothing ->
            if List.length state.publicGames >= settings.maxPublicChats then
                ( state
                , Just <|
                    ErrorRsp
                        { kind = TooManyPublicGamesError
                        , message =
                            "There may not be more than "
                                ++ toString settings.maxPublicChats
                                ++ " public chats."
                        }
                )
            else if String.length chatName == 0 then
                ( state
                , Just <|
                    ErrorRsp
                        { kind = InvalidPublicGameNameError
                        , message =
                            "Public chat name may not be blank."
                        }
                )
            else
                newPublicChatReqInternal state memberName chatName


newPublicChatReqInternal : ServerState -> MemberName -> GameId -> ( ServerState, Maybe Message )
newPublicChatReqInternal state memberName chatName =
    let
        ( memberid, state2 ) =
            ServerInterface.newPlayerid state

        gamestate =
            { members = [ ( memberid, memberName ) ] }

        state3 =
            ServerInterface.addGame chatName gamestate state2

        info =
            { gameid = chatName
            , player = memberName
            }

        state4 =
            ServerInterface.addPlayer memberid info state3

        publicGame =
            { gameid = chatName
            , playerName = memberName
            }
    in
    ( { state4
        | publicGames =
            ServerInterface.appendPublicGames publicGame
                state4.publicGames
      }
    , Just <|
        JoinChatRsp
            { chatid = chatName
            , memberid = Just memberid
            , memberName = memberName
            , otherMembers = []
            , isPublic = True
            }
    )


newChatReq : ServerState -> MemberName -> ( ServerState, Maybe Message )
newChatReq state memberName =
    if gameCount state >= settings.maxChats then
        ( state
        , Just <|
            ErrorRsp
                { kind = TooManyGamesError
                , message =
                    "There may not be more than "
                        ++ toString settings.maxChats
                        ++ " active chats."
                }
        )
    else
        newChatReqInternal state memberName


newChatReqInternal : ServerState -> MemberName -> ( ServerState, Maybe Message )
newChatReqInternal state memberName =
    let
        ( chatid, state2 ) =
            ServerInterface.newGameid state

        ( memberid, state3 ) =
            ServerInterface.newPlayerid state2

        gamestate =
            { members = [ ( memberid, memberName ) ] }

        state4 =
            ServerInterface.addGame chatid gamestate state3

        info =
            { gameid = chatid
            , player = memberName
            }
    in
    ( ServerInterface.addPlayer memberid info state4
    , Just <|
        JoinChatRsp
            { chatid = chatid
            , memberid = Just memberid
            , memberName = memberName
            , otherMembers = []
            , isPublic = False
            }
    )
