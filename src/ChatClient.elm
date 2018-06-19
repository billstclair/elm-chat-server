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
        , p
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (disabled, href, size, style, type_, value)
import Html.Events exposing (onClick, onInput)
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
    , memberName : MemberName
    , serverInterface : ServerInterface GameState Player Message Msg
    , chatid : GameId
    , memberid : PlayerId
    , otherMembers : List MemberName
    , isPublic : Bool
    }


type alias Model =
    { settings : ElmChat.Settings Msg
    , chats : List ChatInfo
    , currentChat : Maybe ChatInfo
    , pendingChat : Maybe ChatInfo
    , memberName : String
    , chatName : String
    , chatid : String
    , error : Maybe String
    }


type Msg
    = ChatUpdate (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend String (ElmChat.Settings Msg)
    | SetMemberName String
    | SetChatName String
    | SetChatid String
    | NewChat
    | JoinChat
    | Receive (ServerInterface GameState Player Message Msg) Message


init : ( Model, Cmd Msg )
init =
    ( { settings = ElmChat.makeSettings "id1" 14 True ChatUpdate
      , chats = []
      , currentChat = Nothing
      , pendingChat = Nothing
      , memberName = "Nobody"
      , chatName = "chat"
      , chatid = ""
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMemberName name ->
            { model | memberName = name } ! []

        SetChatName name ->
            { model | chatName = name } ! []

        SetChatid id ->
            { model | chatid = id } ! []

        NewChat ->
            case model.currentChat of
                Just _ ->
                    { model
                        | error = Just "Can't join multiple chats in local mode."
                    }
                        ! []

                Nothing ->
                    let
                        server =
                            makeProxyServer messageProcessor Receive

                        info =
                            { chatName = model.chatName
                            , memberName = model.memberName
                            , serverInterface = server
                            , chatid = ""
                            , memberid = ""
                            , otherMembers = []
                            , isPublic = False
                            }
                    in
                    { model | pendingChat = Just info }
                        ! [ send server <|
                                NewChatReq { memberName = model.memberName }
                          ]

        JoinChat ->
            model ! []

        ChatUpdate settings cmd ->
            ( { model | settings = settings }
            , cmd
            )

        ChatSend line settings ->
            case model.currentChat of
                Nothing ->
                    model ! []

                Just info ->
                    { model | settings = settings }
                        ! [ send info.serverInterface <|
                                SendReq
                                    { memberid = info.memberid
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
                        , error = Nothing
                      }
                    , cmd
                    )

                JoinChatRsp { chatid, memberid, memberName, otherMembers, isPublic } ->
                    case model.pendingChat of
                        Nothing ->
                            { model
                                | error =
                                    Just "Got a join response with no pending request."
                            }
                                ! []

                        Just info ->
                            case memberid of
                                Just id ->
                                    -- It's a new chat
                                    let
                                        info2 =
                                            { info
                                                | serverInterface = interface
                                                , chatid = chatid
                                                , memberid = id
                                                , memberName = memberName
                                                , otherMembers = otherMembers
                                                , isPublic = isPublic
                                            }
                                    in
                                    { model
                                        | currentChat = Just info2
                                        , pendingChat = Nothing
                                        , chats = info2 :: model.chats
                                        , error = Nothing
                                    }
                                        ! []

                                Nothing ->
                                    -- New member for existing chat
                                    model ! []

                _ ->
                    { model | error = Just <| toString message } ! []


b : String -> Html Msg
b string =
    Html.b [] [ text string ]


center : List (Attribute msg) -> List (Html msg) -> Html msg
center attributes body =
    Html.node "center" attributes body


view : Model -> Html Msg
view model =
    center []
        [ h2 [] [ text "Elm Chat" ]
        , p [] [ ElmChat.chat model.settings ]
        , case model.currentChat of
            Nothing ->
                text ""

            Just info ->
                p []
                    [ b (info.memberName ++ ": ")
                    , ElmChat.inputBox
                        40
                        "Send"
                        ChatSend
                        model.settings
                    ]
        , p []
            [ b "Name: "
            , input
                [ type_ "text"
                , value model.memberName
                , onInput SetMemberName
                , size 40
                ]
                []
            , br
            , b "Chat Name: "
            , input
                [ type_ "text"
                , value model.chatName
                , onInput SetChatName
                , size 40
                ]
                []
            , text " "
            , button [ onClick NewChat ]
                [ text "New Chat" ]
            , br
            , b "Chat ID: "
            , input
                [ type_ "text"
                , value model.chatid
                , onInput SetChatid
                , size 40
                ]
                []
            , text " "
            , button [ onClick JoinChat ]
                [ text "Join Chat" ]
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
