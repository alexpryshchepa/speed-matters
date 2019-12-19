port module Elm.Port exposing
    ( getFromLocalStorage
    , pageChanged
    , responseFromLocalStorage
    , saveToLocalStorage
    )

import Json.Encode as Encode


port saveToLocalStorage : ( String, Encode.Value ) -> Cmd msg


port getFromLocalStorage : String -> Cmd msg


port responseFromLocalStorage : (Encode.Value -> msg) -> Sub msg


port pageChanged : String -> Cmd msg
