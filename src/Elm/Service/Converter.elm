module Elm.Service.Converter exposing
    ( paceToSec
    , secToPace
    , secToTime
    , timeToHour
    , timeToSec
    )

import Elm.Service.Calculator as CalculatorService
import Elm.Service.Validator as ValidatorService


timeToSec : String -> Maybe Int
timeToSec value =
    if ValidatorService.isTime value then
        case String.split ":" value |> List.map String.toInt of
            [ Just h, Just min, Just sec ] ->
                Just <| CalculatorService.hToSec h + CalculatorService.minToSec min + sec

            _ ->
                Nothing

    else
        Nothing


timeToHour : String -> Maybe Float
timeToHour value =
    if ValidatorService.isTime value then
        case String.split ":" value |> List.map String.toInt of
            [ Just h, Just min, Just sec ] ->
                Just <| toFloat h + CalculatorService.minToH (toFloat min) + CalculatorService.secToH sec

            _ ->
                Nothing

    else
        Nothing


secToTime : Int -> String
secToTime sec =
    let
        hour =
            floor <| CalculatorService.secToH sec

        min =
            floor <| CalculatorService.secToMin (sec - CalculatorService.hToSec hour)

        toString int =
            if int < 10 then
                "0" ++ String.fromInt int

            else
                String.fromInt int
    in
    String.concat
        [ toString hour
        , ":"
        , toString min
        , ":"
        , toString <| sec - (CalculatorService.hToSec hour + CalculatorService.minToSec min)
        ]


paceToSec : String -> Maybe Int
paceToSec value =
    if ValidatorService.isPace value then
        case String.split ":" value |> List.map String.toInt of
            [ Just min, Just sec ] ->
                Just <| CalculatorService.minToSec min + sec

            _ ->
                Nothing

    else
        Nothing


secToPace : Int -> String
secToPace sec =
    let
        min =
            floor <| CalculatorService.secToMin sec

        toString int =
            if int < 10 then
                "0" ++ String.fromInt int

            else
                String.fromInt int
    in
    String.concat
        [ toString min
        , ":"
        , toString <| sec - CalculatorService.minToSec min
        ]
