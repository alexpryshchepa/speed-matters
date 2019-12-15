module Elm.Service.Validator exposing
    ( floatRegex
    , intRegex
    , isFloat
    , isInt
    , isPace
    , isTime
    , paceRegex
    , timeRegex
    )

import Regex


intRegex =
    "^\\d+$"


floatRegex =
    "^\\d+(\\.\\d+)?$"


timeRegex =
    "^(\\d+):[0-5][0-9]:[0-5][0-9]$"


paceRegex =
    "^(\\d+):[0-5][0-9]$"


isValueValid : String -> String -> Bool
isValueValid regex value =
    (Regex.fromString regex
        |> Maybe.withDefault Regex.never
        |> Regex.contains
    )
        value


isFloat : String -> Bool
isFloat =
    isValueValid floatRegex


isInt : String -> Bool
isInt =
    isValueValid intRegex


isTime : String -> Bool
isTime =
    isValueValid timeRegex


isPace : String -> Bool
isPace =
    isValueValid paceRegex
