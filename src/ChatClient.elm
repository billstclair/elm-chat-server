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
import ChatClient.Types exposing (GameState, MemberName, Message(..), Player)
import Debug exposing (log)
import ElmChat
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
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
    , isPublic : Bool
    , state : String
    }


type alias Model =
    { settings : ElmChat.Settings Msg
    , chats : List ChatInfo
    , error : Maybe String
    }


type Msg
    = ChatUpdate (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend String String (ElmChat.Settings Msg)


init : ( Model, Cmd Msg )
init =
    ( { settings = ElmChat.makeSettings "id1" 14 True ChatUpdate
      , chats = []
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChatUpdate settings cmd ->
            ( { model | settings = log "update" settings }
            , cmd
            )

        ChatSend name line settings ->
            let
                ( settings1, cmd ) =
                    ElmChat.addChat settings (name ++ ": " ++ line)
            in
            ( { model | settings = settings1 }
            , cmd
            )


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
        , p []
            [ table []
                [ tr []
                    [ td [] [ b "Bill: " ]
                    , td []
                        [ ElmChat.inputBox
                            40
                            "Send"
                            (ChatSend "Bill")
                            model.settings
                        ]
                    ]
                ]
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
            , br [] []
            , a [ href "https://gibgoygames.com/" ]
                [ text "Gib Goy Games" ]
            , text " "
            , a [ href "https://github.com/billstclair/elm-chat-server" ]
                [ text "GitHub" ]
            ]
        ]


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
