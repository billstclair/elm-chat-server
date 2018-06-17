---------------------------------------------------------------------
--
-- Example.elm
-- Top-level shared example UI for WebSocket client/server framework.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Example exposing (..)

import Debug exposing (log)
import ExampleInterface
    exposing
        ( GameState
        , Message(..)
        , Player
        , messageDecoder
        , messageEncoder
        , messageProcessor
        )
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
        , span
        , table
        , td
        , text
        , tr
        )
import Html.Attributes exposing (disabled, href, size, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (Value)
import WebSocket
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)
import WebSocketFramework.ServerInterface as ServerInterface
    exposing
        ( fullMessageProcessor
        , makeProxyServer
        , makeServer
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , MessageDecoder
        , MessageEncoder
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { interface : ServerInterface GameState Player Message Msg
    , urlString : String
    , serverUrl : Maybe String
    , gameid : String
    , playerid : String
    , name : String
    , x : Int
    , y : Int
    , result : String
    , messages : List String
    }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = messageEncoder
    , decoder = messageDecoder
    , errorWrapper = Nothing
    }


fullProcessor : ServerMessageProcessor GameState Player Message
fullProcessor =
    fullMessageProcessor encodeDecode messageProcessor


init : ( Model, Cmd msg )
init =
    { interface = makeProxyServer fullProcessor (IncomingMessage True)
    , urlString = "ws://localhost:8081/"
    , serverUrl = Nothing
    , gameid = ""
    , playerid = ""
    , name = "Bob"
    , x = 1
    , y = 2
    , result = ""
    , messages = []
    }
        ! []


type Msg
    = IncomingMessage Bool (ServerInterface GameState Player Message Msg) Message
    | WebSocketMessage String
    | SetName String
    | SetX String
    | SetY String
    | SetUrl String
    | Add
    | Multiply
    | ToggleConnection
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncomingMessage addToMessages interface message ->
            case message of
                ResultMessage result ->
                    { model
                        | result = result
                        , interface = interface
                        , messages =
                            if addToMessages then
                                let
                                    text =
                                        encodeMessage messageEncoder message
                                in
                                ("recv: " ++ text) :: model.messages
                            else
                                model.messages
                    }
                        ! []

                _ ->
                    model ! []

        WebSocketMessage string ->
            let
                model2 =
                    { model | messages = ("sock: " ++ string) :: model.messages }
            in
            case decodeMessage messageDecoder string of
                Err msg ->
                    { model2 | messages = ("err:  " ++ string) :: model2.messages }
                        ! []

                Ok message ->
                    update (IncomingMessage False model2.interface message) model2

        SetName name ->
            { model | name = name } ! []

        SetX str ->
            case String.toInt str of
                Ok x ->
                    { model | x = x } ! []

                Err _ ->
                    model ! []

        SetY str ->
            case String.toInt str of
                Ok y ->
                    { model | y = y } ! []

                Err _ ->
                    model ! []

        SetUrl str ->
            { model | urlString = str } ! []

        Add ->
            send model (AddMessage model.x model.y)

        Multiply ->
            send model (MultiplyMessage model.x model.y)

        ToggleConnection ->
            let
                disconnect =
                    model.serverUrl /= Nothing
            in
            { model
                | serverUrl =
                    if disconnect then
                        Nothing
                    else
                        Just model.urlString
                , interface =
                    if disconnect then
                        makeProxyServer fullProcessor (IncomingMessage True)
                    else
                        makeServer messageEncoder model.urlString Noop
            }
                ! []

        Noop ->
            model ! []


send : Model -> Message -> ( Model, Cmd Msg )
send model message =
    let
        text =
            encodeMessage messageEncoder message

        model2 =
            { model | messages = ("send: " ++ text) :: model.messages }
    in
    model2 ! [ ServerInterface.send model.interface message ]


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    let
        connected =
            model.serverUrl /= Nothing
    in
    div [ style [ ( "margin-left", "2em" ) ] ]
        [ h2 [] [ text "WebSocketFramework Example" ]
        , p []
            [ table []
                [ tr []
                    [ td [] [ text "x:" ]
                    , td []
                        [ input
                            [ onInput SetX
                            , value <| toString model.x
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] [ text "y:" ]
                    , td []
                        [ input
                            [ onInput SetY
                            , value <| toString model.y
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] []
                    , td []
                        [ button [ onClick Add ]
                            [ text "Add" ]
                        , text " "
                        , button [ onClick Multiply ]
                            [ text "Multiply" ]
                        ]
                    ]
                , tr []
                    [ td [] [ text "Result: " ]
                    , td [] [ text model.result ]
                    ]
                ]
            ]
        , p []
            [ text "URL: "
            , input
                [ onInput SetUrl
                , value model.urlString
                , disabled connected
                , size 30
                ]
                []
            , text " "
            , button [ onClick ToggleConnection ]
                [ text <|
                    if connected then
                        "Disconnect"
                    else
                        "Connect"
                ]
            ]
        , p []
            (List.map textAndBr model.messages)
        ]


textAndBr : String -> Html Msg
textAndBr string =
    span [] [ text string, br ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.serverUrl of
        Nothing ->
            Sub.none

        Just url ->
            WebSocket.listen url WebSocketMessage
