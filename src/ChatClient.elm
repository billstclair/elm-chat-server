----------------------------------------------------------------------
--
-- ChatClient.elm
-- The client side of a chat client/server demo for billstclair/elm-websocket-framework
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ChatClient exposing (..)

import Char
import ChatClient.Interface exposing (messageProcessor)
import ChatClient.Types exposing (GameState, MemberName, Message(..), Player)
import Debug exposing (log)
import Dict exposing (Dict)
import ElmChat
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h2
        , input
        , option
        , p
        , select
        , table
        , text
        )
import Html.Attributes
    exposing
        ( colspan
        , disabled
        , href
        , selected
        , size
        , style
        , type_
        , value
        )
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JD exposing (Decoder)
import List.Extra as LE
import WebSocketFramework exposing (makeProxyServer, makeServer, send)
import WebSocketFramework.Types exposing (GameId, PlayerId, ServerInterface)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias ChatInfo =
    { chatName : String
    , memberNames : List MemberName
    , server : ServerInterface GameState Player Message Msg
    , chatid : GameId
    , memberids : List PlayerId
    , otherMembers : List MemberName
    , isPublic : Bool
    , settings : ElmChat.Settings Msg
    }


type alias Model =
    { settings : ElmChat.Settings Msg
    , proxyServer : ServerInterface GameState Player Message Msg
    , chats : Dict GameId ChatInfo
    , currentChat : Maybe ChatInfo
    , pendingChat : Maybe ChatInfo
    , memberName : String
    , chatName : String
    , chatid : String
    , gameCount : Int
    , error : Maybe String
    }


type Msg
    = ChatUpdate (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend PlayerId String (ElmChat.Settings Msg)
    | SetMemberName String
    | SetChatName String
    | SetChatid String
    | ChangeChat String
    | NewChat
    | JoinChat
    | LeaveChat PlayerId
    | Receive (ServerInterface GameState Player Message Msg) Message


emptySettings : ElmChat.Settings Msg
emptySettings =
    ElmChat.makeSettings "id1" 14 True ChatUpdate


init : ( Model, Cmd Msg )
init =
    ( { settings = emptySettings
      , proxyServer = makeProxyServer messageProcessor Receive
      , chats = Dict.empty
      , currentChat = Nothing
      , pendingChat = Nothing
      , memberName = "Nobody"
      , chatName = "chat"
      , chatid = ""
      , gameCount = 0
      , error = Nothing
      }
    , Cmd.none
    )


newCurrentChat : Model -> ChatInfo -> Maybe ChatInfo
newCurrentChat model info =
    case model.currentChat of
        Nothing ->
            Nothing

        Just chat ->
            if chat.chatid == info.chatid then
                Just info
            else
                model.currentChat


updateChats : Model -> Dict GameId ChatInfo
updateChats model =
    case model.currentChat of
        Nothing ->
            model.chats

        Just chat ->
            Dict.insert
                chat.chatid
                { chat
                    | settings =
                        model.settings
                }
                model.chats


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMemberName name ->
            { model | memberName = name } ! []

        SetChatName name ->
            { model | chatName = name } ! []

        SetChatid id ->
            { model | chatid = id } ! []

        ChangeChat chatid ->
            case Dict.get chatid model.chats of
                Nothing ->
                    -- Can't happen
                    model ! []

                Just info ->
                    { model
                        | currentChat = Just info
                        , settings = info.settings
                        , chats = updateChats model
                    }
                        ! []

        NewChat ->
            case model.currentChat of
                Just _ ->
                    { model
                        | error = Just "Can't join multiple chats in local mode."
                    }
                        ! []

                Nothing ->
                    let
                        gameCount =
                            model.gameCount

                        server =
                            model.proxyServer

                        info =
                            { chatName = model.chatName
                            , memberNames = [ model.memberName ]
                            , server = server
                            , chatid = ""
                            , memberids = []
                            , otherMembers = []
                            , isPublic = False
                            , settings = emptySettings
                            }
                    in
                    { model | pendingChat = Just info }
                        ! [ send server <|
                                NewChatReq { memberName = model.memberName }
                          ]

        JoinChat ->
            model ! []

        LeaveChat memberid ->
            case model.currentChat of
                Nothing ->
                    model ! []

                Just info ->
                    model
                        ! [ send info.server <|
                                LeaveChatReq { memberid = memberid }
                          ]

        ChatUpdate settings cmd ->
            ( { model | settings = settings }
            , cmd
            )

        ChatSend memberid line settings ->
            case model.currentChat of
                Nothing ->
                    model ! []

                Just info ->
                    { model | settings = settings }
                        ! [ send info.server <|
                                SendReq
                                    { memberid = memberid
                                    , message = line
                                    }
                          ]

        Receive interface message ->
            case log "message" message of
                ReceiveRsp { chatid, memberName, message } ->
                    let
                        ( settings1, cmd ) =
                            ElmChat.addChat model.settings
                                (memberName ++ ": " ++ message)
                    in
                    ( { model
                        | settings = settings1
                        , proxyServer = interface
                        , error = Nothing
                      }
                    , cmd
                    )

                JoinChatRsp { chatid, memberid, memberName, otherMembers, isPublic } ->
                    case memberid of
                        Just id ->
                            -- It's a new chat, or a second member
                            case model.pendingChat of
                                Nothing ->
                                    { model
                                        | error =
                                            Just "Got a join response with no pending request."
                                    }
                                        ! []

                                Just info ->
                                    let
                                        info2 =
                                            { info
                                                | server = log "interface" interface
                                                , chatid = chatid
                                                , memberids = [ id ]
                                                , memberNames = [ memberName ]
                                                , otherMembers = otherMembers
                                                , isPublic = isPublic
                                            }

                                        settings =
                                            info2.settings

                                        chats =
                                            updateChats model
                                    in
                                    { model
                                        | settings = settings
                                        , proxyServer = interface
                                        , currentChat = Just info2
                                        , pendingChat = Nothing
                                        , chats =
                                            Dict.insert chatid info2 chats
                                        , chatid = chatid
                                        , error = Nothing
                                    }
                                        ! []

                        Nothing ->
                            -- New member for existing chat
                            case Dict.get chatid model.chats of
                                Nothing ->
                                    { model
                                        | error =
                                            Just "JoinChatRsp for unknown chat."
                                    }
                                        ! []

                                Just info ->
                                    let
                                        info2 =
                                            { info
                                                | otherMembers =
                                                    memberName :: info.otherMembers
                                            }
                                    in
                                    { model
                                        | chats =
                                            Dict.insert chatid info2 model.chats
                                        , currentChat =
                                            newCurrentChat model info2
                                    }
                                        ! []

                LeaveChatRsp { chatid, memberName } ->
                    case Dict.get chatid model.chats of
                        Nothing ->
                            { model
                                | error =
                                    Just "LeaveChatRsp received for unknown chat."
                            }
                                ! []

                        Just info ->
                            if not <| List.member memberName info.memberNames then
                                -- Another member left
                                let
                                    info2 =
                                        { info
                                            | otherMembers =
                                                LE.remove memberName info.otherMembers
                                        }
                                in
                                { model
                                    | chats =
                                        Dict.insert chatid info2 model.chats
                                    , currentChat =
                                        newCurrentChat model info2
                                }
                                    ! []
                            else
                                let
                                    members =
                                        LE.remove memberName info.memberNames

                                    ( chats, current, settings ) =
                                        if members == [] then
                                            let
                                                chats =
                                                    Dict.remove chatid model.chats

                                                current =
                                                    List.head <| Dict.values chats
                                            in
                                            ( chats
                                            , current
                                            , case current of
                                                Nothing ->
                                                    emptySettings

                                                Just chat ->
                                                    chat.settings
                                            )
                                        else
                                            ( Dict.insert
                                                chatid
                                                { info | memberNames = members }
                                                model.chats
                                            , model.currentChat
                                            , model.settings
                                            )
                                in
                                { model
                                    | currentChat = current
                                    , chats = chats
                                    , settings = settings
                                    , proxyServer = interface
                                }
                                    ! []

                _ ->
                    { model | error = Just <| toString message } ! []


b : String -> Html Msg
b string =
    Html.b [] [ text string ]


center : List (Attribute msg) -> List (Html msg) -> Html msg
center attributes body =
    Html.node "center" attributes body


styles : String -> Html Msg
styles css =
    Html.node "style" [] [ text css ]


view : Model -> Html Msg
view model =
    center []
        [ styles "th { text-align: right }"
        , h2 [] [ text "Elm Chat" ]
        , p [] [ ElmChat.chat model.settings ]
        , table [] <|
            List.concat
                [ currentChatRows model
                , [ tr [ td [ br ] ] ]
                , newChatRows model
                ]
        , p [ style [ ( "color", "red" ) ] ]
            [ case model.error of
                Nothing ->
                    text nbsp

                Just msg ->
                    text msg
            ]
        , p []
            [ text <| "Copyright " ++ copyright ++ " 2018 Bill St. Clair"
            , br
            , a [ href "https://gibgoygames.com/" ]
                [ text "Gib Goy Games" ]
            , text " "
            , a [ href "https://github.com/billstclair/elm-chat-server" ]
                [ text "GitHub" ]
            ]
        ]


tr : List (Html Msg) -> Html Msg
tr body =
    Html.tr [] body


th : String -> Html Msg
th string =
    Html.th [] [ text string ]


td : List (Html Msg) -> Html Msg
td body =
    Html.td [] body


currentChatRows : Model -> List (Html Msg)
currentChatRows model =
    case model.currentChat of
        Nothing ->
            []

        Just info ->
            List.concat
                [ inputRows model info
                , [ tr
                        [ th "Chat: "
                        , td [ chatSelector model info ]
                        , case info.memberids of
                            [ id ] ->
                                td
                                    [ button [ onClick <| LeaveChat id ]
                                        [ text "Leave" ]
                                    ]

                            _ ->
                                text ""
                        ]
                  , tr
                        [ th "ID: "
                        , td
                            [ input
                                [ type_ "text"
                                , value info.chatid
                                , size 40
                                , disabled True
                                ]
                                []
                            ]
                        ]
                  , case info.otherMembers of
                        [] ->
                            text ""

                        members ->
                            tr
                                [ th "Members:"
                                , td [ text <| String.join ", " members ]
                                ]
                  ]
                ]


inputRows : Model -> ChatInfo -> List (Html Msg)
inputRows model info =
    let
        onlyone =
            Just [] == List.tail info.memberNames
    in
    List.map2
        (\id name ->
            tr
                [ th <| name ++ ": "
                , Html.td [ colspan 2 ]
                    [ ElmChat.inputBox
                        40
                        "Send"
                        (ChatSend id)
                        model.settings
                    ]
                , if onlyone then
                    text ""
                  else
                    td
                        [ button [ onClick <| LeaveChat id ]
                            [ text "Leave" ]
                        ]
                ]
        )
        info.memberids
        info.memberNames


onChange : (String -> msg) -> Attribute msg
onChange msg =
    on "change" (JD.map msg targetValue)


chatSelector : Model -> ChatInfo -> Html Msg
chatSelector model info =
    let
        chats =
            Dict.values model.chats

        chatid =
            info.chatid
    in
    select
        [ style [ ( "width", "100%" ) ]
        , onChange ChangeChat
        ]
    <|
        List.map
            (\chat ->
                option
                    [ selected <| chatid == chat.chatid
                    , value chat.chatid
                    ]
                    [ text chat.chatName ]
            )
            chats


newChatRows : Model -> List (Html Msg)
newChatRows model =
    [ tr
        [ th "Name: "
        , td
            [ input
                [ type_ "text"
                , value model.memberName
                , onInput SetMemberName
                , size 40
                ]
                []
            ]
        ]
    , tr
        [ th "Chat Name: "
        , td
            [ input
                [ type_ "text"
                , value model.chatName
                , onInput SetChatName
                , size 40
                ]
                []
            ]
        , td
            [ button [ onClick NewChat ]
                [ text "New" ]
            ]
        ]
    , tr
        [ th "Chat ID: "
        , td
            [ input
                [ type_ "text"
                , value model.chatid
                , onInput SetChatid
                , size 40
                ]
                []
            ]
        , td
            [ button [ onClick JoinChat ]
                [ text "Join" ]
            ]
        ]
    ]


br : Html Msg
br =
    Html.br [] []


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


{-| \u00A0
-}
nbsp : String
nbsp =
    stringFromCode 160


{-| \u00A9
-}
copyright : String
copyright =
    stringFromCode 169
