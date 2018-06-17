module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Maybe exposing (withDefault)
import Test exposing (..)
import WebSocketFramework.EncodeDecode as ED exposing (decodePlist)
import WebSocketFramework.Types as Types
    exposing
        ( MessageParser
        , Plist
        , ReqRsp(..)
        )


log =
    Debug.log


enableLogging : Bool
enableLogging =
    False



--change to True to log JSON input & output results


maybeLog : String -> a -> a
maybeLog label value =
    if enableLogging then
        log label value
    else
        value


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


all : Test
all =
    Test.concat <|
        List.concat
            [ testMap protocolTest protocolData
            ]


expectResult : Result String Message -> Result String Message -> Expectation
expectResult sb was =
    case maybeLog "  result" was of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false msg True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


type Message
    = Join String
    | Joined String String
    | Leave
    | Left String String


encoder : Message -> ( ReqRsp, Plist )
encoder message =
    case message of
        Join gameid ->
            ( Req "join", [ ( "gameid", JE.string gameid ) ] )

        Joined gameid playerid ->
            ( Rsp "joined"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              ]
            )

        Leave ->
            ( Req "leave", [] )

        Left gameid playerid ->
            ( Rsp "left"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              ]
            )


parser : ReqRsp -> Plist -> Result String Message
parser reqrsp plist =
    case reqrsp of
        Req msg ->
            case msg of
                "join" ->
                    decodePlist joinDecoder plist

                "leave" ->
                    Ok Leave

                _ ->
                    Err <| "Unknown request: '" ++ msg ++ "'"

        Rsp msg ->
            case msg of
                "joined" ->
                    decodePlist joinedDecoder plist

                "left" ->
                    decodePlist leftDecoder plist

                _ ->
                    Err <| "Unknown response: '" ++ msg ++ "'"


joinDecoder : Decoder Message
joinDecoder =
    JD.map Join
        (JD.field "gameid" JD.string)


joinedDecoder : Decoder Message
joinedDecoder =
    JD.map2 Joined
        (JD.field "gameid" JD.string)
        (JD.field "playerid" JD.string)


leftDecoder : Decoder Message
leftDecoder =
    JD.map2 Left
        (JD.field "gameid" JD.string)
        (JD.field "playerid" JD.string)


protocolTest : Message -> String -> Test
protocolTest message name =
    test ("protocolTest \"" ++ name ++ "\"")
        (\_ ->
            let
                json =
                    maybeLog "protocolJson" <| ED.encodeMessage encoder message
            in
            expectResult (Ok message) <| ED.decodeMessage parser json
        )


protocolData : List Message
protocolData =
    [ Join "<gameid>"
    , Joined "<gameid>" "<playerid>"
    , Leave
    , Left "<gameid>" "<playerid>"
    ]
