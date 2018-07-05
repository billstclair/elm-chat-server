----------------------------------------------------------------------
--
-- PortChat.elm
-- The top-level chat program, using LocalStorage ports.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module PortChat exposing (..)

import ChatClient.SharedUI exposing (Msg, programWithFlags)
import LocalStorage
import LocalStorage.SharedTypes
    exposing
        ( ClearPort
        , GetItemPort
        , ListKeysPort
        , Ports
        , ReceiveItemPort
        , SetItemPort
        )


main =
    programWithFlags ports receiveItem


ports : Ports Msg
ports =
    LocalStorage.makeRealPorts getItem setItem clear listKeys


port getItem : GetItemPort msg


port setItem : SetItemPort msg


port clear : ClearPort msg


port listKeys : ListKeysPort msg


port receiveItem : ReceiveItemPort msg