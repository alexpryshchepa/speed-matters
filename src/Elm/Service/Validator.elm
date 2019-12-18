module Elm.Service.Validator exposing
    ( fieldEmpty
    , fieldNotValid
    , fieldsEmpty
    , fieldsNotValid
    , floatError
    , floatHint
    , floatRegex
    , intRegex
    , integerError
    , integerHint
    , isFloat
    , isInt
    , isPace
    , isTime
    , outOfRange
    , paceError
    , paceHint
    , paceRegex
    , timeError
    , timeHint
    , timeRegex
    , unhandledException
    )

import Regex
import String.Extra as StringExtra


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



-- Hints & Errors


outOfRange : String -> String
outOfRange name =
    name ++ " calculation is out of range, please insert smaller values" |> StringExtra.toSentenceCase


fieldsNotValid : String
fieldsNotValid =
    "Inserted values are not valid, please take a note at the hints under the fields"


fieldNotValid : String -> String
fieldNotValid name =
    name ++ " is not valid, make sure you follow the pattern you can find under the field" |> StringExtra.toSentenceCase


fieldsEmpty : String -> String
fieldsEmpty name =
    "The fields are empty. Please fill the values to calculate " ++ name


fieldEmpty : String -> String
fieldEmpty name =
    name ++ " field is empty. Please insert a value" |> StringExtra.toSentenceCase


floatHint : String
floatHint =
    "Must be a number, float number is allowed (e.g. 21.5, 100)"


floatError : String
floatError =
    "Wrong value. Please make sure the value is an integer or a float (e.g. 42.195, 2000)"


integerHint : String
integerHint =
    "Must be an integer (e.g. 42, 100)"


integerError : String
integerError =
    "Wrong value. Please make sure the value is an integer (e.g. 55, 1000)"


paceHint : String
paceHint =
    "Must follow MM:SS pattern (e.g. 04:35)"


paceError : String
paceError =
    "Wrong value. Please make sure you added leading zeros and the value in MM:SS format (e.g. 06:05)"


timeHint : String
timeHint =
    "Must follow HH:MM:SS pattern (e.g. 02:01:59)"


timeError : String
timeError =
    "Wrong value. Please make sure you added leading zeros and the value in HH:MM:SS format (e.g. 03:59:53)"


unhandledException : String
unhandledException =
    "Something went wrong. Please contact us"
