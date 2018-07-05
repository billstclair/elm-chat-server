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
