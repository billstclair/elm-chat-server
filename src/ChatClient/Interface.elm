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
        , messageToGameid
        , messageToPlayerid
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
            newChatReq state memberName Nothing

        NewPublicChatReq { memberName, chatName } ->
            newPublicChatReq state memberName chatName

        JoinChatReq { chatid, memberName } ->
            joinChatReq state chatid memberName

        RejoinChatReq { memberid, chatid, memberName, isPublic } ->
            rejoinChatReq state memberid chatid memberName isPublic

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
                                    List.filter (\( id, _ ) -> id /= memberid)
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


joinChatReq : ServerState -> GameId -> MemberName -> ( ServerState, Maybe Message )
joinChatReq state chatid memberName =
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


dot : String
dot =
    "."


removeTrailingDots : String -> String
removeTrailingDots string =
    if String.endsWith dot string then
        removeTrailingDots <| String.dropRight 1 string
    else
        string


countTrailingDots : String -> Int
countTrailingDots string =
    String.length string - String.length (removeTrailingDots string)


maxTrailingDots : Int
maxTrailingDots =
    3


nextMemberName : String -> Maybe String
nextMemberName memberName =
    if countTrailingDots memberName < maxTrailingDots then
        Just <| memberName ++ "."
    else
        Nothing


rejoinChatReq : ServerState -> PlayerId -> GameId -> MemberName -> Bool -> ( ServerState, Maybe Message )
rejoinChatReq state memberid chatid memberName isPublic =
    case Dict.get memberid state.playerDict of
        Nothing ->
            rejoinChatReqInternal state
                chatid
                (removeTrailingDots memberName)
                isPublic

        Just { gameid } ->
            case Dict.get gameid state.gameDict of
                Nothing ->
                    ( state
                    , Just <|
                        ErrorRsp
                            { kind = UnknownMemberidError { memberid = memberid }
                            , message = "This shouldn't happen."
                            }
                    )

                Just gamestate ->
                    let
                        members =
                            List.filter (\( id, _ ) -> id /= memberid)
                                gamestate.members
                    in
                    ( state
                    , Just <|
                        JoinChatRsp
                            { chatid = chatid
                            , memberid = Just memberid
                            , memberName = memberName
                            , otherMembers = List.map Tuple.second members
                            , isPublic = Nothing /= getPublicGame chatid state
                            }
                    )


rejoinChatReqInternal : ServerState -> GameId -> MemberName -> Bool -> ( ServerState, Maybe Message )
rejoinChatReqInternal state chatid memberName isPublic =
    let
        ( state2, msg ) =
            joinChatReq state chatid memberName
    in
    case msg of
        Just (ErrorRsp { kind }) ->
            case kind of
                MemberExistsError _ ->
                    case nextMemberName memberName of
                        Nothing ->
                            rejoinCreateNew state
                                chatid
                                (removeTrailingDots memberName)
                                isPublic

                        Just name ->
                            rejoinChatReqInternal state
                                chatid
                                name
                                isPublic

                _ ->
                    rejoinCreateNew state
                        chatid
                        (removeTrailingDots memberName)
                        isPublic

        _ ->
            ( state2, msg )


rejoinCreateNew : ServerState -> GameId -> MemberName -> Bool -> ( ServerState, Maybe Message )
rejoinCreateNew state chatid memberName isPublic =
    if isPublic then
        newPublicChatReq state memberName chatid
    else
        case Dict.get chatid state.gameDict of
            Nothing ->
                newChatReq state memberName <| Just chatid

            _ ->
                ( state
                , Just <|
                    ErrorRsp
                        { kind = UnknownChatidError { chatid = chatid }
                        , message = "This can't happen"
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


newChatReq : ServerState -> MemberName -> Maybe GameId -> ( ServerState, Maybe Message )
newChatReq state memberName gameid =
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
        newChatReqInternal state memberName gameid


newChatReqInternal : ServerState -> MemberName -> Maybe GameId -> ( ServerState, Maybe Message )
newChatReqInternal state memberName gameid =
    let
        ( chatid, state2 ) =
            case gameid of
                -- We trust our caller, rejoinCreateNew
                Just id ->
                    ( id, state )

                Nothing ->
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


messageToPlayerid : Message -> Maybe PlayerId
messageToPlayerid message =
    case message of
        JoinChatRsp { memberid } ->
            case memberid of
                Nothing ->
                    Nothing

                Just mid ->
                    Just mid

        SendReq { memberid } ->
            Just memberid

        LeaveChatReq { memberid } ->
            Just memberid

        _ ->
            Nothing
