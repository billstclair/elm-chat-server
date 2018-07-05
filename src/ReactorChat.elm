----------------------------------------------------------------------
--
-- ReactorChat.elm
-- The top-level chat program, using simulated LocalStorage ports.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ReactorChat exposing (..)

import ChatClient.SharedUI
    exposing
        ( Msg(..)
        , localStoragePrefix
        , program
        )
import LocalStorage.DictPorts as DictPorts
import LocalStorage.SharedTypes exposing (Ports)


main =
    program ports


ports : Ports Msg
ports =
    DictPorts.make ReceiveLocalStorage localStoragePrefix
