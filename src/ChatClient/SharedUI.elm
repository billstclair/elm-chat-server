----------------------------------------------------------------------
--
-- SharedUI.elm
-- The client side of a chat client/server demo for billstclair/elm-websocket-framework
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ChatClient.SharedUI
    exposing
        ( Msg(..)
        , localStoragePrefix
        , program
        , programWithFlags
        )

{-| TODO

Encrypted chats.

There's a delay in the second member rejoin when multiple members in a chat from the same client.

It's bit confusing to have two separate windows in a single browser. They share the persistence, so can't be really separate. It would be lovely to be able to detect that and do something reasonable, like maybe the second session isn't persistent.

"Mute" another member by clicking on their name in the "Members" list.
"Unmute" by clicking on their name in the (new) "Muted" list.
Persist the "Muted" list for each chat.

Lock private chats so nobody else can join. Should look like the chat doesn't exist to anybody who tries to join. Specify valid usernames, and auto-lock when all have joined.

If the server goes down, LeaveChatReq should time out and clean up the client connection. Timeout sends, too. And joins.

Limit number of participants in a chat to something large, but which will limit DoS attacks at least a little bit.

Enter/Return should auto-press "New" or "Join" button.

Don't delete public chats until necessary to satisfy limit. Admin mode to enable deleting them by hand. Clear creator when he disconnects. Let him delete the public game when he leaves, if nobody else is in it.

Delete an idle private chat with only one member after a long timeout, e.g. an hour.
Or maybe delete it only if somebody tries to make a new one, and we've already reached the max.

Find or build a better styled text area for billstclair/elm-chat. Need at least bold/italics, and links. Should also have a seen line when you switch back into a chat, or back into the browser window. With scrolling doing the right thing to keep the seen line in view.

Write code notes in src/README.md

-}

import Char
import ChatClient.EncodeDecode
    exposing
        ( decodeChatKey
        , decodeSavedModel
        , encodeChatKey
        , messageDecoder
        , messageEncoder
        , savedModelEncoder
        , stringPairDecoder
        , stringPairEncoder
        )
import ChatClient.Interface exposing (messageProcessor, messageToGameid)
import ChatClient.Types
    exposing
        ( ChatKey
        , ErrorKind(..)
        , GameState
        , MemberName
        , MemberNames
        , Message(..)
        , Player
        , PublicChat
        , SavedModel
        , WhichPage(..)
        )
import Date exposing (Date)
import Debug exposing (log)
import Dict exposing (Dict)
import ElmChat
import Formatting as F exposing ((<>), Format)
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
        , span
        , table
        , text
        )
import Html.Attributes
    exposing
        ( checked
        , class
        , colspan
        , disabled
        , href
        , selected
        , size
        , style
        , type_
        , value
        )
import Html.Events exposing (on, onCheck, onClick, onInput, targetValue)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (decode, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import LocalStorage exposing (LocalStorage)
import LocalStorage.SharedTypes as LS
import SystemNotification exposing (NotifyPort, createNotification)
import Task
import Time exposing (Time)
import WebSocket
import WebSocketFramework exposing (makeProxyServer)
import WebSocketFramework.EncodeDecode exposing (decodeMessage)
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , ServerInterface(..)
        , ServerUrl
        )


type alias Server =
    ServerInterface GameState Player Message Msg


{-| New York during Daylight Saving Time
-}
defaultTimezoneOffset : Int
defaultTimezoneOffset =
    240


program : LS.Ports Msg -> Platform.Program Never Model Msg
program ports =
    Html.program
        { init = init defaultTimezoneOffset ports Nothing (\_ -> Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


programWithFlags : LS.Ports Msg -> LS.ReceiveItemPort Msg -> NotifyPort Msg -> Platform.Program Int Model Msg
programWithFlags ports receiveItemPort notify =
    Html.programWithFlags
        { init =
            \timezoneOffset ->
                init timezoneOffset ports (Just receiveItemPort) notify
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


localStoragePrefix : String
localStoragePrefix =
    "ChatClient"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (List.concat
            [ [ case model.receiveItemPort of
                    Nothing ->
                        Sub.none

                    Just receivePort ->
                        receivePort <|
                            LS.receiveWrapper ReceiveLocalStorage localStoragePrefix
              ]
            , Dict.toList model.connectedServers
                |> List.map
                    (\( server, interface ) ->
                        WebSocket.listen server <| WebSocketMessage interface
                    )
            ]
        )


type alias ChatInfo =
    { chatName : String
    , members : List ( PlayerId, MemberName )
    , server : Maybe Server
    , chatid : GameId
    , otherMembers : List MemberName
    , isPublic : Bool
    , settings : ElmChat.Settings Msg
    }


emptyChatKey : ChatKey
emptyChatKey =
    ( "", "" )


chatKeyUrl : ChatKey -> ServerUrl
chatKeyUrl =
    Tuple.first


chatKeyId : ChatKey -> GameId
chatKeyId =
    Tuple.second


chatKey : ChatInfo -> ChatKey
chatKey chat =
    ( case chat.server of
        Nothing ->
            ""

        Just server ->
            serverUrl server
    , chat.chatid
    )


type PendingChat
    = NoPendingChat
    | ExistingPendingChat ChatKey
    | NewPendingChat ChatInfo


type alias Model =
    { restoreState : RestoreState
    , whichPage : WhichPage
    , proxyServer : Server
    , chats : Dict ( ServerUrl, GameId ) ChatInfo
    , publicChats : List PublicChat
    , currentChat : ChatKey
    , pendingChat : PendingChat
    , memberName : String
    , serverUrl : String
    , connectedServers : Dict String Server
    , isRemote : Bool
    , chatName : String
    , chatid : String
    , publicChatName : String
    , hideHelp : Bool
    , activityDict : Dict ChatKey ( String, Int )
    , time : Time
    , timeZoneOffset : Int
    , storage : LocalStorage Msg
    , receiveItemPort : Maybe (LS.ReceiveItemPort Msg)
    , showNotifications : Bool
    , notifyPort : NotifyPort Msg
    , lastNotifyTime : Time
    , error : Maybe String
    }


type Msg
    = Noop
    | SetTime Time
    | DelayedAction (Model -> ( Model, Cmd Msg )) Time
    | SwitchPage WhichPage
    | ChatUpdate (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend PlayerId String (ElmChat.Settings Msg)
    | SetMemberName String
    | SetServerUrl String
    | ReceiveServerLoadFile (Result Http.Error String)
    | SetIsRemote Bool
    | SetChatName String
    | SetChatid String
    | SetPublicChatName String
    | ShowHideHelp
    | ShowHideNotifications
    | ChangeChat String
    | NewChat
    | JoinChat
    | LeaveChat PlayerId
    | ClearChat
    | JoinPublicChat (Maybe GameId)
    | NewPublicChat
    | RefreshPublicChats
    | WebSocketMessage Server String
    | Receive Server Message
    | ReceiveLocalStorage LS.Operation (Maybe (LS.Ports Msg)) LS.Key LS.Value


emptySettings : ElmChat.Settings Msg
emptySettings =
    let
        settings =
            ElmChat.makeSettings "id1" 14 True ChatUpdate
    in
    restoreChatSettingsHeight settings


restoreChatSettingsHeight : ElmChat.Settings Msg -> ElmChat.Settings Msg
restoreChatSettingsHeight settings =
    let
        attributes =
            settings.attributes
    in
    { settings
        | attributes =
            { attributes
                | textArea =
                    List.concat
                        [ attributes.textArea
                        , [ style [ ( "height", "15em" ) ] ]
                        ]
            }
    }


serverLoadFile : String
serverLoadFile =
    "server.txt"


initialProxyServer : Server
initialProxyServer =
    makeProxyServer messageProcessor Receive


init : Int -> LS.Ports Msg -> Maybe (LS.ReceiveItemPort Msg) -> NotifyPort Msg -> ( Model, Cmd Msg )
init timeZoneOffset ports receiveItemPort notify =
    let
        storage =
            LocalStorage.make ports localStoragePrefix
    in
    { restoreState =
        case receiveItemPort of
            Just _ ->
                RestoreStart

            Nothing ->
                RestoreDone
    , whichPage = MainPage
    , proxyServer = initialProxyServer
    , chats = Dict.empty
    , publicChats = []
    , currentChat = emptyChatKey
    , pendingChat = NoPendingChat
    , memberName = "Nobody"
    , serverUrl = "ws://localhost:8081"
    , connectedServers = Dict.empty
    , isRemote = True
    , chatName = "chat"
    , chatid = ""
    , publicChatName = ""
    , hideHelp = False
    , activityDict = Dict.empty
    , time = 0
    , timeZoneOffset = timeZoneOffset
    , storage = storage
    , receiveItemPort = receiveItemPort
    , showNotifications = False
    , notifyPort = notify
    , lastNotifyTime = 0
    , error = Nothing
    }
        ! [ Http.send ReceiveServerLoadFile <| Http.getString serverLoadFile
          , Task.perform SetTime Time.now
          , case receiveItemPort of
                Nothing ->
                    Cmd.none

                Just _ ->
                    LocalStorage.getItem storage
                        (log "restoring model" localStorageModelKey)
          ]


updateChats : ElmChat.Settings Msg -> ChatInfo -> Model -> Model
updateChats settings chat model =
    { model
        | chats =
            Dict.insert (chatKey chat)
                { chat
                    | settings =
                        settings
                }
                model.chats
    }


send : Server -> Model -> Message -> Cmd Msg
send server model message =
    WebSocketFramework.send server <| log "send" message


getServer : Model -> ( Model, Maybe Server )
getServer model =
    let
        url =
            model.serverUrl
    in
    if not model.isRemote then
        ( model, Nothing )
    else
        case Dict.get url model.connectedServers of
            Just server ->
                ( model, Just server )

            Nothing ->
                let
                    server =
                        Maybe.withDefault initialProxyServer <| makeServer url
                in
                ( { model
                    | connectedServers =
                        Dict.insert url server model.connectedServers
                  }
                , Just server
                )


makeServer : ServerUrl -> Maybe Server
makeServer url =
    if url == "" then
        Nothing
    else
        Just <| WebSocketFramework.makeServer messageEncoder url Noop


newChatInfo : Model -> ( Model, ChatInfo )
newChatInfo model =
    let
        ( mdl, server ) =
            getServer model
    in
    ( mdl
    , { chatName = model.chatName
      , members = []
      , server = server
      , chatid = ""
      , otherMembers = []
      , isPublic = False
      , settings = emptySettings
      }
    )


chatServer : ChatInfo -> Model -> Server
chatServer info model =
    case info.server of
        Nothing ->
            model.proxyServer

        Just s ->
            s


localStorageModelKey : LS.Key
localStorageModelKey =
    "model"


localStorageChatKey : ChatKey -> LS.Key
localStorageChatKey key =
    "chat:" ++ encodeChatKey key


saveModel : Model -> Cmd Msg
saveModel model =
    if model.receiveItemPort == Nothing then
        Cmd.none
    else
        modelToSaved model
            |> savedModelEncoder
            |> LocalStorage.setItem model.storage localStorageModelKey


saveChat : ChatInfo -> Model -> Cmd Msg
saveChat info model =
    if model.receiveItemPort == Nothing then
        Cmd.none
    else
        chatEncoder info
            |> LocalStorage.setItem model.storage
                (localStorageChatKey <| chatKey info)


deleteChat : ChatKey -> Model -> Cmd Msg
deleteChat chatkey model =
    if model.receiveItemPort == Nothing then
        Cmd.none
    else
        LocalStorage.setItem model.storage (localStorageChatKey chatkey) JE.null


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( mdl, cmd ) =
            updateInternal msg model
    in
    mdl
        ! [ cmd
          , saveModel mdl
          ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        Noop ->
            model ! []

        SetTime time ->
            { model | time = time, lastNotifyTime = time } ! []

        DelayedAction updater time ->
            updater { model | time = time }

        WebSocketMessage server json ->
            case decodeMessage messageDecoder json of
                Err msg ->
                    { model | error = Just msg } ! []

                Ok message ->
                    update (Receive server message) model

        Receive interface message ->
            receive interface message model

        ReceiveLocalStorage operation ports key value ->
            receiveLocalStorage operation key value model

        ChatUpdate settings cmd ->
            case Dict.get model.currentChat model.chats of
                Nothing ->
                    model ! []

                Just chat ->
                    let
                        chat2 =
                            { chat | settings = settings }
                    in
                    { model
                        | chats =
                            Dict.insert model.currentChat
                                chat2
                                model.chats
                    }
                        ! [ cmd
                          , saveChat chat2 model
                          ]

        _ ->
            updateUIChanges msg { model | lastNotifyTime = model.time }


updateUIChanges : Msg -> Model -> ( Model, Cmd Msg )
updateUIChanges msg model =
    case msg of
        SetMemberName name ->
            { model | memberName = name } ! []

        SetServerUrl url ->
            { model | serverUrl = url } ! []

        SetIsRemote isRemote ->
            let
                mdl =
                    { model | isRemote = isRemote }
            in
            if mdl.whichPage == PublicChatsPage then
                update RefreshPublicChats mdl
            else
                mdl ! []

        SetChatName name ->
            { model | chatName = name } ! []

        SetChatid id ->
            { model | chatid = id } ! []

        SetPublicChatName name ->
            { model | publicChatName = name } ! []

        ShowHideHelp ->
            { model | hideHelp = not model.hideHelp } ! []

        ShowHideNotifications ->
            let
                showNotifications =
                    not model.showNotifications
            in
            { model | showNotifications = showNotifications }
                ! [ if showNotifications then
                        createNotification
                            model.notifyPort
                            "You have successfully enabled notifications."
                            Nothing
                            Nothing
                    else
                        Cmd.none
                  ]

        ReceiveServerLoadFile result ->
            case result of
                Err error ->
                    model ! []

                Ok url ->
                    { model | serverUrl = url } ! []

        SwitchPage whichPage ->
            if whichPage == model.whichPage then
                model ! []
            else
                switchPage whichPage model

        ChangeChat json ->
            case decodeChatKey json of
                Err msg ->
                    { model
                        | error =
                            Just <|
                                "Can't change to chat "
                                    ++ toString json
                    }
                        ! []

                Ok chatkey ->
                    case Dict.get chatkey model.chats of
                        Nothing ->
                            { model
                                | error =
                                    Just <|
                                        "Unknown chat: ChangeChat "
                                            ++ toString chatkey
                            }
                                ! []

                        Just { chatName, settings } ->
                            ({ model
                                | currentChat = chatkey
                                , chatid = chatKeyId chatkey
                                , error = Nothing
                             }
                                |> incrementActivityCount chatkey "" 0
                            )
                                ! [ ElmChat.restoreScroll settings ]

        NewChat ->
            let
                ( mdl, info ) =
                    newChatInfo model

                server =
                    chatServer info mdl
            in
            { mdl
                | pendingChat = NewPendingChat info
                , error = Nothing
            }
                ! [ send server model <|
                        NewChatReq
                            { memberName = model.memberName }
                  ]

        JoinChat ->
            joinChat model.chatid model

        LeaveChat memberid ->
            case Dict.get model.currentChat model.chats of
                Nothing ->
                    model ! []

                Just info ->
                    let
                        server =
                            chatServer info model
                    in
                    model
                        ! [ send server model <|
                                LeaveChatReq
                                    { memberid = memberid }
                          ]

        ClearChat ->
            case Dict.get model.currentChat model.chats of
                Nothing ->
                    model ! []

                Just info ->
                    let
                        chat =
                            { info
                                | settings = emptySettings
                            }
                    in
                    { model
                        | chats =
                            Dict.insert model.currentChat
                                chat
                                model.chats
                    }
                        ! [ saveChat chat model ]

        JoinPublicChat maybeChatName ->
            let
                chatName =
                    case maybeChatName of
                        Just name ->
                            name

                        Nothing ->
                            model.publicChatName
            in
            joinChat chatName { model | chatName = chatName }

        NewPublicChat ->
            let
                ( mdl, info ) =
                    newChatInfo
                        { model
                            | chatName =
                                model.publicChatName
                        }

                server =
                    chatServer info mdl
            in
            { mdl
                | pendingChat = NewPendingChat info
                , error = Nothing
            }
                ! [ send server model <|
                        NewPublicChatReq
                            { memberName = model.memberName
                            , chatName = model.publicChatName
                            }
                  ]

        RefreshPublicChats ->
            let
                ( mdl2, maybeServer ) =
                    getServer model

                server =
                    case maybeServer of
                        Nothing ->
                            model.proxyServer

                        Just s ->
                            s
            in
            mdl2 ! [ send server mdl2 GetPublicChatsReq ]

        ChatSend memberid line settings ->
            case Dict.get model.currentChat model.chats of
                Nothing ->
                    model ! []

                Just info ->
                    let
                        server =
                            chatServer info model
                    in
                    { model
                        | chats =
                            Dict.insert model.currentChat
                                { info | settings = settings }
                                model.chats
                        , error = Nothing
                    }
                        ! [ send server model <|
                                SendReq
                                    { memberid = memberid
                                    , message = line
                                    }
                          ]

        _ ->
            model ! []


incrementActivityCount : ChatKey -> String -> Int -> Model -> Model
incrementActivityCount chatkey chatName amount model =
    if amount == 0 then
        { model
            | activityDict = Dict.remove chatkey model.activityDict
        }
    else
        let
            tuple =
                case Dict.get chatkey model.activityDict of
                    Nothing ->
                        ( chatName, amount )

                    Just ( n, a ) ->
                        ( n, a + amount )
        in
        { model
            | activityDict = Dict.insert chatkey tuple model.activityDict
        }


updateActivity : Message -> Server -> Model -> Model
updateActivity message server model =
    case message of
        JoinChatRsp { chatid } ->
            let
                chatkey =
                    serverChatKey chatid server
            in
            if chatkey == model.currentChat then
                model
            else
                case Dict.get chatkey model.chats of
                    Nothing ->
                        model

                    Just { chatName } ->
                        incrementActivityCount chatkey chatName 1 model

        LeaveChatRsp { chatid, memberName } ->
            let
                chatkey =
                    serverChatKey chatid server
            in
            case Dict.get chatkey model.chats of
                Nothing ->
                    model

                Just { chatName, members } ->
                    case members of
                        [ ( _, name ) ] ->
                            if name == memberName then
                                incrementActivityCount chatkey chatName 0 model
                            else if chatkey /= model.currentChat then
                                incrementActivityCount chatkey chatName 1 model
                            else
                                model

                        _ ->
                            if chatkey /= model.currentChat then
                                incrementActivityCount chatkey chatName 1 model
                            else
                                model

        ReceiveRsp { chatid } ->
            let
                chatkey =
                    serverChatKey chatid server
            in
            if chatkey == model.currentChat then
                model
            else
                case Dict.get chatkey model.chats of
                    Nothing ->
                        model

                    Just { chatName } ->
                        incrementActivityCount chatkey chatName 1 model

        _ ->
            model


receive : Server -> Message -> Model -> ( Model, Cmd Msg )
receive interface message model =
    let
        mdl =
            (if isProxyServer interface then
                { model | proxyServer = interface }
             else
                model
            )
                |> updateActivity message interface
    in
    case log "Receive" message of
        ReceiveRsp { chatid, memberName, message } ->
            receiveRsp chatid memberName message interface mdl

        JoinChatRsp { chatid, memberid, memberName, otherMembers, isPublic } ->
            joinChatRsp chatid memberid memberName otherMembers isPublic interface mdl

        LeaveChatRsp { chatid, memberName } ->
            leaveChatRsp chatid memberName interface mdl

        GetPublicChatsRsp { chats } ->
            { mdl | publicChats = chats }
                ! []

        ErrorRsp error ->
            case model.restoreState of
                RestoreDone ->
                    { mdl
                        | error = Just error.message
                        , pendingChat = NoPendingChat
                    }
                        ! []

                _ ->
                    continueRestore model

        _ ->
            { mdl
                | error = Just <| toString message
                , pendingChat = NoPendingChat
            }
                ! []


delayedAction : (Model -> ( Model, Cmd Msg )) -> Cmd Msg
delayedAction updater =
    Task.perform (DelayedAction updater) Time.now


receiveRsp : GameId -> MemberName -> String -> Server -> Model -> ( Model, Cmd Msg )
receiveRsp chatid memberName message server model =
    model
        ! [ delayedAction <|
                receiveRspDelayed chatid
                    memberName
                    message
                    server
          ]


receiveRspDelayed : GameId -> MemberName -> String -> Server -> Model -> ( Model, Cmd Msg )
receiveRspDelayed chatid memberName message server model =
    let
        chatkey =
            serverChatKey chatid server
    in
    case Dict.get chatkey model.chats of
        Nothing ->
            model ! []

        Just chat ->
            let
                ( settings1, cmd ) =
                    ElmChat.addChat chat.settings <|
                        timestamp model
                            ++ " "
                            ++ memberName
                            ++ ": "
                            ++ message

                chat2 =
                    { chat | settings = settings1 }

                mdl =
                    { model
                        | chats =
                            Dict.insert chatkey chat2 model.chats
                        , error = Nothing
                    }

                isRemote =
                    Nothing
                        == LE.find (\( _, name ) -> name == memberName)
                            chat.members

                ( mdl2, cmd2 ) =
                    if isRemote then
                        notify (memberName ++ " talked in " ++ chat.chatName)
                            mdl
                    else
                        ( mdl, Cmd.none )
            in
            mdl2
                ! [ if chatkey == model.currentChat then
                        cmd
                    else
                        Cmd.none
                  , saveChat chat2 mdl
                  , cmd2
                  ]


minIdleTimeBeforeNotification : Time
minIdleTimeBeforeNotification =
    1.0 * Time.minute


notify : String -> Model -> ( Model, Cmd Msg )
notify message model =
    let
        time =
            model.time

        lastNotifyTime =
            model.lastNotifyTime

        diff =
            log "Ticks until notify" <|
                time
                    - (lastNotifyTime + minIdleTimeBeforeNotification)
    in
    if model.showNotifications && diff > 0 then
        { model | lastNotifyTime = model.time }
            ! [ createNotification model.notifyPort
                    "Elm Chat"
                    (Just message)
                    Nothing
              ]
    else
        model ! []


joinChatRsp : GameId -> Maybe PlayerId -> MemberName -> MemberNames -> Bool -> Server -> Model -> ( Model, Cmd Msg )
joinChatRsp chatid memberid memberName otherMembers isPublic server model =
    case model.restoreState of
        RestoreDone ->
            model
                ! [ delayedAction <|
                        joinChatRspDelayed chatid
                            memberid
                            memberName
                            otherMembers
                            isPublic
                            server
                  ]

        _ ->
            let
                ( mdl, cmd ) =
                    joinChatRspDelayed chatid
                        memberid
                        memberName
                        otherMembers
                        isPublic
                        server
                        model

                ( mdl2, cmd2 ) =
                    continueRestore mdl
            in
            mdl2 ! [ cmd, cmd2 ]


joinChatRspDelayed : GameId -> Maybe PlayerId -> MemberName -> MemberNames -> Bool -> Server -> Model -> ( Model, Cmd Msg )
joinChatRspDelayed chatid memberid memberName otherMembers isPublic server model =
    let
        chatkey =
            serverChatKey chatid server
    in
    case memberid of
        Nothing ->
            -- Remote member just joined. Ignore model.pendingChat
            case Dict.get chatkey model.chats of
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

                        ( settings, cmd ) =
                            ElmChat.addChat info2.settings <|
                                timestamp model
                                    ++ " "
                                    ++ memberName
                                    ++ " joined the chat."

                        info3 =
                            { info2 | settings = settings }

                        mdl =
                            { model
                                | chats =
                                    Dict.insert chatkey info3 model.chats
                                , error = Nothing
                            }
                    in
                    mdl
                        ! [ switchPageCmd MainPage
                          , if chatkey == model.currentChat then
                                cmd
                            else
                                Cmd.none
                          , saveChat info3 mdl
                          ]

        Just id ->
            let
                mdl =
                    { model | pendingChat = NoPendingChat }
            in
            case model.pendingChat of
                NoPendingChat ->
                    { mdl
                        | error =
                            Just "JoinChatRsp for without pending chat."
                    }
                        ! []

                ExistingPendingChat key ->
                    if key /= chatkey then
                        { mdl
                            | error =
                                Just <|
                                    "Requested join of chat: "
                                        ++ toString key
                                        ++ ", got: "
                                        ++ toString chatkey
                        }
                            ! []
                    else
                        -- New local member for existing chat
                        case Dict.get chatkey mdl.chats of
                            Nothing ->
                                { mdl
                                    | error =
                                        Just <|
                                            "Can't find pending chat for (url, id): "
                                                ++ toString chatkey
                                }
                                    ! []

                            Just info ->
                                let
                                    info2 =
                                        { info
                                            | members =
                                                ( id, memberName ) :: info.members
                                        }

                                    mdl2 =
                                        { mdl
                                            | chats =
                                                Dict.insert chatkey info2 mdl.chats
                                        }
                                in
                                mdl2 ! [ saveChat info2 mdl2 ]

                NewPendingChat info ->
                    -- Newly create chat
                    let
                        info2 =
                            { info
                                | chatid = chatid
                                , members = [ ( id, memberName ) ]
                                , otherMembers = otherMembers
                                , isPublic = isPublic
                            }

                        mdl2 =
                            { mdl
                                | chats =
                                    Dict.insert chatkey info2 mdl.chats
                                , currentChat = chatkey
                                , chatid = chatid
                            }
                    in
                    mdl2
                        ! [ switchPageCmd MainPage
                          , saveChat info2 mdl2
                          ]


leaveChatRsp : GameId -> MemberName -> Server -> Model -> ( Model, Cmd Msg )
leaveChatRsp chatid memberName server model =
    model
        ! [ delayedAction <| leaveChatRspDelayed chatid memberName server ]


leaveChatRspDelayed : GameId -> MemberName -> Server -> Model -> ( Model, Cmd Msg )
leaveChatRspDelayed chatid memberName server model =
    let
        chatkey =
            serverChatKey chatid server
    in
    case Dict.get chatkey model.chats of
        Nothing ->
            { model
                | error =
                    Just "LeaveChatRsp received for unknown chat."
            }
                ! []

        Just info ->
            let
                names =
                    List.map Tuple.second info.members
            in
            if not <| List.member memberName names then
                -- Another member left
                let
                    ( settings, cmd ) =
                        ElmChat.addChat info.settings <|
                            timestamp model
                                ++ " "
                                ++ memberName
                                ++ " left the chat."

                    info2 =
                        { info
                            | settings = settings
                            , otherMembers =
                                LE.remove memberName info.otherMembers
                        }

                    mdl =
                        { model
                            | chats =
                                Dict.insert chatkey info2 model.chats
                            , error = Nothing
                        }
                in
                mdl
                    ! [ if chatkey == model.currentChat then
                            cmd
                        else
                            Cmd.none
                      , saveChat info2 mdl
                      ]
            else
                -- A local member left
                let
                    members =
                        LE.filterNot
                            (\( _, name ) ->
                                name == memberName
                            )
                            info.members

                    ( chats, current, servers, cmd ) =
                        if members == [] then
                            let
                                chats =
                                    Dict.remove chatkey model.chats

                                newChat =
                                    List.head <| Dict.values chats

                                servers =
                                    computeConnectedServers chats

                                ( current, cmd ) =
                                    case newChat of
                                        Nothing ->
                                            ( emptyChatKey, Cmd.none )

                                        Just chat ->
                                            ( chatKey chat
                                            , ElmChat.restoreScroll chat.settings
                                            )
                            in
                            ( chats
                            , case newChat of
                                Nothing ->
                                    emptyChatKey

                                Just chat ->
                                    chatKey chat
                            , servers
                            , Cmd.batch [ cmd, deleteChat chatkey model ]
                            )
                        else
                            let
                                chat =
                                    { info
                                        | members = members
                                    }
                            in
                            ( Dict.insert
                                chatkey
                                chat
                                model.chats
                            , chatkey
                            , model.connectedServers
                            , saveChat chat model
                            )
                in
                { model
                    | currentChat = current
                    , chats = chats
                    , connectedServers = servers
                    , error = Nothing
                    , chatid =
                        if current == emptyChatKey then
                            model.chatid
                        else
                            chatKeyId current
                }
                    ! [ cmd ]


switchPageCmd : WhichPage -> Cmd Msg
switchPageCmd whichPage =
    Task.perform SwitchPage (Task.succeed whichPage)


switchPage : WhichPage -> Model -> ( Model, Cmd Msg )
switchPage whichPage model =
    let
        isPublic =
            whichPage == PublicChatsPage

        ( mdl, maybeServer ) =
            if isPublic && model.isRemote then
                getServer model
            else
                ( { model
                    | connectedServers =
                        computeConnectedServers model.chats
                  }
                , Nothing
                )

        server =
            case maybeServer of
                Nothing ->
                    model.proxyServer

                Just s ->
                    s

        mdl2 =
            { mdl
                | whichPage = whichPage
                , publicChats = []
                , error = Nothing
            }
    in
    mdl2
        ! [ if isPublic then
                send server mdl GetPublicChatsReq
            else
                case Dict.get model.currentChat model.chats of
                    Nothing ->
                        Cmd.none

                    Just info ->
                        ElmChat.restoreScroll info.settings
          ]


serverChatKey : GameId -> Server -> ChatKey
serverChatKey chatid server =
    ( if isProxyServer server then
        ""
      else
        serverUrl server
    , chatid
    )


currentServerChatKey : GameId -> Model -> ChatKey
currentServerChatKey chatid model =
    ( if model.isRemote then
        model.serverUrl
      else
        ""
    , chatid
    )


{-| This needs to handle timeout. Especially during startup.
-}
joinChat : GameId -> Model -> ( Model, Cmd Msg )
joinChat chatid model =
    let
        chatkey =
            currentServerChatKey chatid model

        ( ( mdl, info ), existing ) =
            case Dict.get chatkey model.chats of
                Just chat ->
                    ( ( model, chat ), True )

                Nothing ->
                    ( newChatInfo model, False )

        server =
            chatServer info mdl
    in
    { mdl
        | pendingChat =
            if existing then
                ExistingPendingChat chatkey
            else
                NewPendingChat info
        , error = Nothing
    }
        ! [ send server mdl <|
                JoinChatReq
                    { chatid = chatid
                    , memberName = model.memberName
                    }
          ]


serverUrl : Server -> String
serverUrl server =
    ServerInterface.getServer server


isProxyServer : Server -> Bool
isProxyServer server =
    "" == serverUrl server


computeConnectedServers : Dict ChatKey ChatInfo -> Dict String Server
computeConnectedServers chats =
    let
        serverInfo : ChatInfo -> Maybe ( String, Server )
        serverInfo =
            \{ server } ->
                case server of
                    Nothing ->
                        Nothing

                    Just interface ->
                        case interface of
                            ServerInterface record ->
                                Just ( record.server, interface )
    in
    List.foldl
        (\info dict ->
            case serverInfo info of
                Nothing ->
                    dict

                Just ( url, interface ) ->
                    Dict.insert url interface dict
        )
        Dict.empty
    <|
        Dict.values chats


b : String -> Html Msg
b string =
    Html.b [] [ text string ]


center : List (Attribute msg) -> List (Html msg) -> Html msg
center attributes body =
    Html.node "center" attributes body


styles : String -> Html Msg
styles css =
    Html.node "style" [] [ text css ]


styleSheet : String
styleSheet =
    """
th {
  text-align: right
}
/* An attractive table style that I've been using for years. */
table.prettytable {
  margin: 0em 0.5em 0.5em 0.5em;
  background: whitesmoke;
  border-collapse: collapse;
}
table.prettytable th, table.prettytable td {
  border: 1px silver solid;
  padding: 0.2em;
}
table.prettytable th {
  background: gainsboro;
  text-align: center;
}
table.prettytable caption {
  margin-left: inherit;
  margin-right: inherit;
}
"""


hourFormat : Format Date String
hourFormat =
    F.padLeft 2 '0' (Date.hour >> F.int)


minuteFormat : Format Date String
minuteFormat =
    F.padLeft 2 '0' (Date.minute >> F.int)


secondFormat : Format Date String
secondFormat =
    F.padLeft 2 '0' (Date.second >> F.int)


timeStampFormat : Format Date String
timeStampFormat =
    hourFormat <> F.s ":" <> minuteFormat <> F.s ":" <> secondFormat


timeFormat : Format Date String
timeFormat =
    hourFormat <> F.s ":" <> minuteFormat


timestamp : Model -> String
timestamp model =
    let
        time =
            model.time - toFloat model.timeZoneOffset

        date =
            Date.fromTime time
    in
    F.print timeStampFormat date


time : Model -> String
time model =
    let
        time =
            model.time - toFloat model.timeZoneOffset

        date =
            Date.fromTime time
    in
    F.print timeFormat date


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "font-size", "14pt" )
            , ( "margin", "1em auto" )
            ]
        ]
        [ center []
            [ styles styleSheet
            , h2 []
                [ text "Elm Chat" ]
            , pageSelector model
            , case model.whichPage of
                MainPage ->
                    viewMainPage model

                PublicChatsPage ->
                    viewPublicChatsPage model
            , p []
                [ text <| "Copyright " ++ copyright ++ " 2018 Bill St. Clair"
                , br
                , a [ href "https://xossbow.com/" ]
                    [ text "Xossbow" ]
                , text " "
                , a [ href "https://github.com/billstclair/elm-chat-server" ]
                    [ text "GitHub" ]
                ]
            ]
        ]


maybeLink : Bool -> String -> Msg -> Html Msg
maybeLink hasLink string msg =
    if hasLink then
        a
            [ href "#"
            , onClick msg
            ]
            [ text string ]
    else
        b string


pageSelector : Model -> Html Msg
pageSelector model =
    let
        isMain =
            model.whichPage == MainPage

        isPublicChat =
            model.whichPage == PublicChatsPage
    in
    div []
        [ maybeLink (not isMain) "Chat" <| SwitchPage MainPage
        , text " "
        , maybeLink (not isPublicChat) "Public" <| SwitchPage PublicChatsPage
        ]


showHideHelpButton : Model -> Html Msg
showHideHelpButton model =
    span []
        [ button [ onClick ShowHideHelp ]
            [ text <|
                if model.hideHelp then
                    "Show Help"
                else
                    "Hide Help"
            ]
        , text " "
        , button [ onClick ShowHideNotifications ]
            [ text <|
                if model.showNotifications then
                    "Hide Notifications"
                else
                    "Show Notifications"
            ]
        ]


errorLine : Model -> Html Msg
errorLine model =
    case model.error of
        Nothing ->
            text ""

        Just msg ->
            p [ style [ ( "color", "red" ) ] ]
                [ text msg ]


viewMainPage : Model -> Html Msg
viewMainPage model =
    let
        settings =
            case Dict.get model.currentChat model.chats of
                Nothing ->
                    emptySettings

                Just info ->
                    info.settings
    in
    div []
        [ p [] [ ElmChat.chat settings ]
        , table [] <|
            List.concat
                [ currentChatRows model
                , newChatRows model
                ]
        , errorLine model
        , showHideHelpButton model
        , if model.hideHelp then
            text ""
          else
            div [ style [ ( "width", "40em" ) ] ]
                [ p []
                    [ text "To start a new chat, fill in your 'Name' and a 'Chat Name' (your local name for the chat, not sent to the server), and click the 'New' button. Then you can fill in the box at the top labelled with your name and type Enter/Return or click the 'Send' button to chat. Give the 'ID' to other people so they can join the chat with you." ]
                , p []
                    [ text "To leave the chat, click the 'Leave' button." ]
                , p []
                    [ text "To join an existing chat, enter your 'Name', paste the 'Chat ID', and click the 'Join' button. You may enter a chat multiple times with different names, and an input box will appear at the top for each member." ]
                , p []
                    [ text "You may join as many chats as you wish. To switch between them, select the one you want from the 'Chat' selector." ]
                , p []
                    [ text "Click the 'Public' link at the top of the page to go to the public chats page. Click 'Chat' from there to come back here."
                    ]
                , p []
                    [ text "The chat 'Server' defaults to the server running on the machine from which you loaded this page. You can change it, if you know of another one. To restore the default, reload this page. If you uncheck the box next to the 'Server', the chat will run locally in your browser, and you can talk to yourself (this is a development testing mode)." ]
                , p []
                    [ text "To hide this help, click the 'Hide Help' button. To toggle giving notifications on receiving a message after being idle for a minute or more, click 'Show Notifications' or 'Hide Notifications'. The first time you click 'Show Notifications', your browser will ask whether to allow notifications. If you do, you'll see a test notification. If you don't, you won't see any notifications until you go into preferences in your browser and enable them."
                    ]
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
    case Dict.get model.currentChat model.chats of
        Nothing ->
            []

        Just info ->
            List.concat
                [ inputRows model info
                , [ tr
                        [ th "Chat: "
                        , td [ chatSelector model info ]
                        , case info.members of
                            [ ( id, _ ) ] ->
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
                        , td
                            [ button [ onClick ClearChat ]
                                [ text "Clear Transcript" ]
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
                  , if 2 > Dict.size model.chats then
                        text ""
                    else
                        tr
                            [ th "Unseen Activity:"
                            , Html.td [ colspan 2 ]
                                [ text <| activityString model ]
                            ]
                  , tr [ td [ br ] ]
                  ]
                ]


activityString : Model -> String
activityString model =
    case Dict.values model.activityDict of
        [] ->
            "none"

        values ->
            values
                |> List.sortBy (Tuple.first >> String.toLower)
                |> List.foldl
                    (\( name, count ) res ->
                        let
                            res2 =
                                if res == "" then
                                    res
                                else
                                    res ++ ", "
                        in
                        res2 ++ name ++ " (" ++ toString count ++ ")"
                    )
                    ""


inputRows : Model -> ChatInfo -> List (Html Msg)
inputRows model info =
    let
        onlyone =
            Just [] == List.tail info.members
    in
    List.map
        (\( id, name ) ->
            tr
                [ th <| name ++ ": "
                , Html.td [ colspan 3 ]
                    [ ElmChat.inputBox
                        40
                        "Send"
                        (ChatSend id)
                        info.settings
                    , if onlyone then
                        text ""
                      else
                        span []
                            [ text " "
                            , button [ onClick <| LeaveChat id ]
                                [ text "Leave" ]
                            ]
                    ]
                ]
        )
        info.members


onChange : (String -> msg) -> Attribute msg
onChange msg =
    on "change" (JD.map msg targetValue)


chatSelector : Model -> ChatInfo -> Html Msg
chatSelector model info =
    let
        chats =
            Dict.values model.chats

        chatkey =
            chatKey info
    in
    select
        [ style [ ( "width", "100%" ) ]
        , onChange ChangeChat
        ]
    <|
        List.map
            (\chat ->
                let
                    body =
                        case Dict.get (chatKey chat) model.activityDict of
                            Nothing ->
                                text chat.chatName

                            Just ( _, count ) ->
                                b <|
                                    chat.chatName
                                        ++ " ("
                                        ++ toString count
                                        ++ ")"

                    key =
                        chatKey chat
                in
                option
                    [ selected <| chatkey == key
                    , value <| encodeChatKey key
                    ]
                    [ body ]
            )
            (List.sortBy (.chatName >> String.toLower) chats)


nameRow : Model -> Html Msg
nameRow model =
    tr
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


serverRow : Model -> Html Msg
serverRow model =
    tr
        [ th "Server:"
        , td
            [ input
                [ type_ "text"
                , value model.serverUrl
                , onInput SetServerUrl
                , size 40
                , disabled (not model.isRemote)
                ]
                []
            ]
        , td
            [ input
                [ type_ "checkbox"
                , checked model.isRemote
                , onCheck SetIsRemote
                ]
                []
            ]
        ]


newChatRows : Model -> List (Html Msg)
newChatRows model =
    [ nameRow model
    , serverRow model
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


viewPublicChatsPage : Model -> Html Msg
viewPublicChatsPage model =
    div []
        [ p []
            [ table []
                [ nameRow model
                , serverRow model
                , tr
                    [ th "Chat Name: "
                    , td
                        [ input
                            [ type_ "text"
                            , value model.publicChatName
                            , onInput SetPublicChatName
                            , size 40
                            ]
                            []
                        ]
                    , td
                        [ button [ onClick <| JoinPublicChat Nothing ]
                            [ text "Join" ]
                        , text " "
                        , button [ onClick NewPublicChat ]
                            [ text "New" ]
                        ]
                    ]
                , tr
                    [ th ""
                    , td [ text "" ]
                    , td
                        [ button [ onClick RefreshPublicChats ]
                            [ text "Refresh" ]
                        ]
                    ]
                ]
            ]
        , p []
            [ case model.publicChats of
                [] ->
                    text "There are no public chats."

                chats ->
                    publicChatsTable model chats
            ]
        , errorLine model
        , showHideHelpButton model
        , if model.hideHelp then
            text ""
          else
            div [ style [ ( "width", "40em" ) ] ]
                [ if model.publicChats == [] then
                    text ""
                  else
                    p []
                        [ text "To join a public chat, fill in your 'Name', then either fill in the 'Chat Name' and click 'Join' or click on one of the underlined names in the 'Chat Name' column of the table."
                        ]
                , p []
                    [ text "To create and join a new chat, fill in your 'Name' and the 'Chat Name' and click 'New'." ]
                , p []
                    [ text "The 'Server' and its check mark are as on the 'Chat' page." ]
                , p []
                    [ text "Click the 'Chat' link at the top of the page to go to the chat page. Click 'Public' from there to come back here."
                    ]
                ]
        ]


publicChatsTable : Model -> List PublicChat -> Html Msg
publicChatsTable model chats =
    table [ class "prettytable" ] <|
        List.concat
            [ [ tr
                    [ th "Chat Name"
                    , th "Created by"
                    , th "Members"
                    ]
              ]
            , List.sortBy (.chatName >> String.toLower) chats
                |> List.map
                    (\chat ->
                        tr
                            [ td
                                [ a
                                    [ href "#"
                                    , onClick <| JoinPublicChat (Just chat.chatName)
                                    ]
                                    [ text chat.chatName ]
                                ]
                            , td [ text chat.memberName ]
                            , td [ text <| toString chat.memberCount ]
                            ]
                    )
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



{- Persistence -}


modelToSaved : Model -> SavedModel
modelToSaved model =
    { whichPage = model.whichPage
    , chatKeys = Dict.keys model.chats
    , currentChat = model.currentChat
    , memberName = model.memberName
    , serverUrl = model.serverUrl
    , isRemote = model.isRemote
    , chatName = model.chatName
    , chatid = model.chatid
    , publicChatName = model.publicChatName
    , hideHelp = model.hideHelp
    , showNotifications = model.showNotifications
    }


savedToModel : SavedModel -> Model -> Model
savedToModel savedModel defaults =
    let
        chat =
            case Dict.get savedModel.currentChat defaults.chats of
                Nothing ->
                    case List.head <| Dict.keys defaults.chats of
                        Nothing ->
                            emptyChatKey

                        Just key ->
                            key

                Just _ ->
                    savedModel.currentChat
    in
    { defaults
        | whichPage = savedModel.whichPage
        , currentChat = chat
        , memberName = savedModel.memberName
        , serverUrl = savedModel.serverUrl
        , isRemote = savedModel.isRemote
        , chatName = savedModel.chatName
        , chatid = savedModel.chatid
        , publicChatName = savedModel.publicChatName
        , hideHelp = savedModel.hideHelp
        , showNotifications = savedModel.showNotifications
    }



{- Encoder and decoder for ChatInfo.

   These are here, instead of EncodeDecode.elm,
   so that the server doesn't need ElmChat or Http.

-}


chatEncoder : ChatInfo -> Value
chatEncoder chat =
    JE.object
        [ ( "chatName", JE.string chat.chatName )
        , ( "members"
          , JE.list <|
                List.map stringPairEncoder chat.members
          )
        , ( "serverUrl"
          , case chat.server of
                Nothing ->
                    JE.null

                Just server ->
                    JE.string <| ServerInterface.getServer server
          )
        , ( "chatid", JE.string chat.chatid )
        , ( "isPublic", JE.bool chat.isPublic )
        , ( "settings", ElmChat.settingsEncoder chat.settings )
        ]


memberDecoder : Decoder ( GameId, MemberName )
memberDecoder =
    stringPairDecoder "member (id, name)"


decodeChat : Value -> Result String ChatInfo
decodeChat value =
    JD.decodeValue chatDecoder value


chatDecoder : Decoder ChatInfo
chatDecoder =
    decode ChatInfo
        |> required "chatName" JD.string
        |> required "members" (JD.list memberDecoder)
        |> optional "serverUrl" (JD.map makeServer JD.string) Nothing
        |> required "chatid" JD.string
        -- otherMembers
        |> hardcoded []
        |> required "isPublic" JD.bool
        |> required "settings"
            (JD.map restoreChatSettingsHeight <|
                ElmChat.settingsDecoder ChatUpdate
            )



{- Restoring from saved state -}


type RestoreState
    = RestoreStart
    | RestoreReadChats
        { key : ChatKey
        , keys : List ChatKey
        , chats : List ChatInfo
        , savedModel : SavedModel
        }
    | RestoreReconnectChats
        { waiting : ChatInfo
        , chats : List ChatInfo
        , savedModel : SavedModel
        }
    | RestoreDone


{-| Sprechen Sie Lisp?
-}
cdr : List a -> List a
cdr list =
    case List.tail list of
        Nothing ->
            []

        Just tail ->
            tail


receiveLocalStorage : LS.Operation -> LS.Key -> LS.Value -> Model -> ( Model, Cmd Msg )
receiveLocalStorage operation key value model =
    let
        op =
            log ("receiveLocalStorage " ++ toString operation ++ " " ++ key) value
    in
    case operation of
        LS.GetItemOperation ->
            let
                nextChat : SavedModel -> List ChatInfo -> List ChatKey -> ( Model, Cmd Msg )
                nextChat =
                    \savedModel chats chatKeys ->
                        case List.head chatKeys of
                            Nothing ->
                                let
                                    -- Reverse member lists to preserve order on screen
                                    chats2 =
                                        List.map
                                            (\chat ->
                                                { chat
                                                    | members = List.reverse chat.members
                                                }
                                            )
                                            chats
                                in
                                startReconnectingChats savedModel chats2 model

                            Just key ->
                                ( { model
                                    | restoreState =
                                        RestoreReadChats
                                            { key = key
                                            , keys = cdr chatKeys
                                            , chats = chats
                                            , savedModel = savedModel
                                            }
                                  }
                                , LocalStorage.getItem
                                    model.storage
                                  <|
                                    log "Restoring chat" <|
                                        localStorageChatKey key
                                )
            in
            case model.restoreState of
                RestoreStart ->
                    if value == JE.null then
                        { model | restoreState = RestoreDone } ! []
                    else
                        case decodeSavedModel value of
                            Err msg ->
                                { model
                                    | error =
                                        Just <|
                                            "Can't decode saved model:"
                                                ++ msg
                                }
                                    ! []

                            Ok mdl ->
                                let
                                    ( mdl2, cmd ) =
                                        nextChat mdl [] mdl.chatKeys
                                in
                                mdl2 ! [ cmd ]

                RestoreReadChats { key, keys, chats, savedModel } ->
                    let
                        ( chats2, mdl ) =
                            if value == JE.null then
                                ( chats, model )
                            else
                                case decodeChat value of
                                    Err _ ->
                                        -- Should probably accumulate errors
                                        ( chats, model )

                                    Ok chat ->
                                        ( chat :: chats
                                        , { model
                                            | chats = Dict.insert key chat model.chats
                                          }
                                        )

                        ( mdl2, cmd ) =
                            nextChat savedModel chats2 keys
                    in
                    mdl2
                        ! [ cmd

                          -- Delete stored chat
                          , LocalStorage.setItem mdl.storage
                                (localStorageChatKey key)
                                JE.null
                          ]

                _ ->
                    model ! []

        _ ->
            model ! []


reconnectToChat : ChatInfo -> Model -> ( Model, Cmd Msg )
reconnectToChat chat model =
    let
        ( server, mdl ) =
            case chat.server of
                Nothing ->
                    ( model.proxyServer, model )

                Just s ->
                    case Dict.get (serverUrl s) model.connectedServers of
                        Nothing ->
                            ( s
                            , { model
                                | connectedServers =
                                    Dict.insert (serverUrl s)
                                        s
                                        model.connectedServers
                              }
                            )

                        Just s2 ->
                            ( s2, model )
    in
    case chat.members of
        [] ->
            ( mdl, Cmd.none )

        ( memberid, memberName ) :: rest ->
            let
                chatkey =
                    chatKey chat

                pendingChat =
                    case Dict.get chatkey mdl.chats of
                        Nothing ->
                            NewPendingChat chat

                        Just info ->
                            ExistingPendingChat chatkey
            in
            { mdl
                | pendingChat = pendingChat
                , restoreState =
                    case mdl.restoreState of
                        RestoreReconnectChats record ->
                            RestoreReconnectChats
                                { record
                                    | waiting =
                                        { chat | members = rest }
                                }

                        state ->
                            state
            }
                ! [ send server model <|
                        RejoinChatReq
                            { memberid = memberid
                            , chatid = chat.chatid
                            , memberName = memberName
                            , isPublic = chat.isPublic
                            }
                  ]


startReconnectingChats : SavedModel -> List ChatInfo -> Model -> ( Model, Cmd Msg )
startReconnectingChats savedModel chats model =
    let
        ( restoreState, chat ) =
            case chats of
                [] ->
                    ( RestoreDone, Nothing )

                chat :: rest ->
                    ( RestoreReconnectChats
                        { waiting = chat
                        , chats = rest
                        , savedModel = savedModel
                        }
                    , Just chat
                    )

        mdl =
            { model | restoreState = restoreState }
    in
    case chat of
        Nothing ->
            ( savedToModel savedModel mdl, Cmd.none )

        Just c ->
            reconnectToChat c mdl


{-| We got an ErrorRsp or JoinChatRsp while rejoining saved chats.

If the rejoin was successful, need to restore that chat's history.

In either case, need to send a RejoinChatReq on the next saved member or chat.

-}
continueRestore : Model -> ( Model, Cmd Msg )
continueRestore model =
    case model.restoreState of
        RestoreReconnectChats { savedModel, waiting, chats } ->
            let
                chatkey =
                    chatKey waiting

                ( mdl, maybeChat ) =
                    let
                        mc =
                            Dict.get chatkey model.chats
                    in
                    case mc of
                        Nothing ->
                            ( model, Nothing )

                        Just chat ->
                            let
                                c =
                                    { chat | settings = waiting.settings }
                            in
                            ( { model
                                | chats =
                                    Dict.insert chatkey c model.chats
                              }
                            , Just c
                            )

                chats2 =
                    if maybeChat == Nothing then
                        chats
                    else if waiting.members /= [] then
                        waiting :: chats
                    else
                        chats

                ( mdl2, cmd ) =
                    startReconnectingChats savedModel chats2 mdl
            in
            mdl2
                ! [ cmd
                  , case maybeChat of
                        Nothing ->
                            Cmd.none

                        Just chat ->
                            saveChat chat mdl2
                  ]

        _ ->
            model
                ! []
