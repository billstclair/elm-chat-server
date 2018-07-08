module Tests exposing (all)

import ChatClient.EncodeDecode exposing (messageDecoder, messageEncoder)
import ChatClient.Types as Types
    exposing
        ( ErrorKind(..)
        , Message(..)
        )
import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Maybe exposing (withDefault)
import Test exposing (..)
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)


log =
    Debug.log


{-| change to True to log JSON input & output results
-}
enableLogging : Bool
enableLogging =
    False


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


encode : Message -> String
encode message =
    encodeMessage messageEncoder message


decode : String -> Result String Message
decode string =
    decodeMessage messageDecoder string


protocolTest : Message -> String -> Test
protocolTest message name =
    test ("protocolTest \"" ++ name ++ "\"")
        (\_ ->
            let
                json =
                    maybeLog "protocolJson" <| encode message
            in
            expectResult (Ok message) <| decode json
        )


protocolData : List Message
protocolData =
    [ PingReq { message = "Hello, World!" }
    , PongRsp { message = "Hello, World!" }
    , NewChatReq { memberName = "Bill" }
    , NewPublicChatReq
        { memberName = "Bill"
        , chatName = "Anarchy"
        }
    , JoinChatReq
        { chatid = "Anarchy"
        , memberName = "John"
        }
    , RejoinChatReq
        { memberid = "123skidoo"
        , chatid = "Anarchy"
        , memberName = "John"
        , isPublic = True
        }
    , JoinChatRsp
        { chatid = "Anarchy"
        , memberid = Just "123skidoo"
        , memberName = "John"
        , otherMembers = [ "Bill" ]
        , isPublic = True
        }
    , JoinChatRsp
        { chatid = "Anarchy"
        , memberid = Nothing
        , memberName = "Lysander"
        , otherMembers = []
        , isPublic = True
        }
    , SendReq
        { memberid = "123skidoo"
        , message = "Muh Roads!!"
        }
    , ReceiveRsp
        { chatid = "Anarchy"
        , memberName = "John"
        , message = "Muh Roads!!"
        }
    , LeaveChatReq { memberid = "123skidoo" }
    , LeaveChatRsp
        { chatid = "Anarchy"
        , memberName = "John"
        }
    , GetPublicChatsReq
    , GetPublicChatsRsp
        { chats =
            [ { memberName = "Bill"
              , chatName = "Anarchy"
              , memberCount = 1000
              }
            , { memberName = "billstclair"
              , chatName = "Gab"
              , memberCount = 1000000
              }
            ]
        }
    , ErrorRsp
        { kind =
            JsonDecodeError
                { messageText = "Frobluate"
                , decodingError = "Frobulate? Are you daft?"
                }
        , message = "Json decode error"
        }
    , ErrorRsp
        { kind =
            PublicChatNameExistsError
                { chatName = "Anarchy" }
        , message = "Public chat name exists"
        }
    , ErrorRsp
        { kind =
            UnknownChatidError
                { chatid = "Gab" }
        , message = "Unknown chatid"
        }
    , ErrorRsp
        { kind =
            MemberExistsError
                { chatid = "Anarchy"
                , memberName = "Bill"
                }
        , message = "Member exists in chat"
        }
    , ErrorRsp
        { kind =
            UnknownMemberidError
                { memberid = "abcdef" }
        , message = "Unknown memberid"
        }
    , ErrorRsp
        { kind = TooManyGamesError
        , message = "Too many games!"
        }
    , ErrorRsp
        { kind = TooManyPublicGamesError
        , message = "Too many public games!"
        }
    , ErrorRsp
        { kind = InvalidPublicGameNameError
        , message = "Wash your mouth right out with soap."
        }
    , ErrorRsp
        { kind =
            UnknownRequestError
                { request = "frobluate"
                }
        , message = "What with the frobulation?"
        }
    ]
