----------------------------------------------------------------------
--
-- Settings.elm
-- Parameters for Elm Chat server.
-- These probably want to be settable with environment variables.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ChatClient.Settings exposing (Settings, settings)

import Time exposing (Time)


type alias Settings =
    { deathRowDuration : Time
    , maxPublicChats : Int
    , maxChats : Int
    }


settings : Settings
settings =
    { deathRowDuration = 30 * Time.second
    , maxPublicChats = 20
    , maxChats = 100
    }
