port module Server exposing (..)

import ChatClient.EncodeDecode
    exposing
        ( messageDecoder
        , messageEncoder
        )
import ChatClient.Interface exposing (messageProcessor)
import ChatClient.Types
    exposing
        ( GameState
        , Message(..)
        , Player
        )
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , UserFunctions
        , WrappedModel
        , program
        , sendToOne
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , GameId
        , InputPort
        , OutputPort
        , ServerState
        )


type alias ServerModel =
    ()


serverModel : ServerModel
serverModel =
    ()


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = messageEncoder
    , decoder = messageDecoder
    , errorWrapper = Nothing
    }


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender model socket state request response =
    ( model, sendToOne messageEncoder response outputPort socket )


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = messageProcessor
    , messageSender = messageSender
    , messageToGameid = Just messageToGameid
    , inputPort = inputPort
    , outputPort = outputPort
    }


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


main =
    program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
