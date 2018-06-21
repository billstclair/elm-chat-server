---------------------------------------------------------------------
--
-- Client.elm
-- Simple low-level client for a WebSocket server
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- This program will NOT run under elm-reactor. It expects a server
-- URL as a flag, so must be run from JavaScript code that supplies that.
-- See the `server` directory for instructions.
--
----------------------------------------------------------------------


module Client exposing (..)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import WebSocket


main : Program String Model Msg
main =
    H.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { messages : List String
    , input : String
    , server : String
    }


init : String -> ( Model, Cmd msg )
init server =
    ( { messages = []
      , input = ""
      , server = server
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputMessage String
    | SubmitMessage
    | ServerMessage String


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        InputMessage value ->
            ( { model | input = value }
            , Cmd.none
            )

        SubmitMessage ->
            ( { model
                | input = ""
                , messages = ("Send: " ++ model.input) :: model.messages
              }
            , WebSocket.send model.server model.input
            )

        ServerMessage message ->
            ( { model
                | messages = ("Recv: " ++ message) :: model.messages
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.server ServerMessage



-- VIEW


onEnter : Msg -> H.Attribute Msg
onEnter message =
    E.on "keydown"
        (E.keyCode |> Decode.andThen (is13 message))


is13 : a -> Int -> Decoder a
is13 a code =
    if code == 13 then
        Decode.succeed a
    else
        Decode.fail "not the right key code"


messageView : String -> Html Msg
messageView message =
    H.li
        []
        [ H.text message ]


br : Html Msg
br =
    H.br [] []


view : Model -> Html Msg
view model =
    H.div
        []
        [ H.h1 [] [ H.text "WebSocket Server Test Client" ]
        , H.ul [] (List.map messageView model.messages)
        , H.p []
            [ H.input
                [ A.type_ "text"
                , A.placeholder "Message..."
                , A.value model.input
                , A.size 50
                , E.onInput InputMessage
                , onEnter SubmitMessage
                ]
                []
            ]
        , H.span []
            [ H.b [] [ H.text "Server: " ]
            , H.text model.server
            , br
            , H.b [] [ H.text "Examples:" ]
            ]
        , H.span [] <|
            List.concatMap
                (\str -> [ br, H.code [] [ H.text str ] ])
                examples
        ]


examples : List String
examples =
    [ """
       ["req","new",{"memberName":"Bill"}]
      """
    , """
       ["req","send",{"memberid":"MEMBERID","message":"Hello, World!"}]
      """
    , """
       ["req","join",{"chatid":"CHATID","memberName":"Bill"}]
      """
    , """
       ["req","leave",{"memberid":"MEMBERID"}]
      """
    , """
       ["req","newPublic",{"memberName":"Bill","chatName":"Elm Programmers"}]
      """
    ]
