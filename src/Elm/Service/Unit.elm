module Elm.Service.Unit exposing
    ( Convertation(..)
    , Distance(..)
    , Pace(..)
    , Unit(..)
    , convert
    )

import Elm.Service.Calculator as CalculatorService
import Elm.Service.Converter as ConverterService
import Maybe.Extra as MaybeExtra
import Regex


type Unit
    = Distance Distance
    | Time
    | Pace Pace


type Distance
    = Kilometer
    | Meter
    | Mile
    | Yard


type Pace
    = PerKilometer
    | PerMile


type Convertation
    = Converted String
    | ConvertationFailed
    | ConvertationSkipped


convert : Unit -> Unit -> String -> Convertation
convert from to value =
    case ( from, to ) of
        ( Distance unitFrom, Distance unitTo ) ->
            convertDistance unitFrom unitTo value

        ( Pace unitFrom, Pace unitTo ) ->
            convertPace unitFrom unitTo value

        _ ->
            ConvertationSkipped


convertDistance : Distance -> Distance -> String -> Convertation
convertDistance from to value =
    case from of
        Kilometer ->
            case to of
                Kilometer ->
                    ConvertationSkipped

                Meter ->
                    Just (String.fromInt << CalculatorService.kmToM)
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> convertationResult

                Mile ->
                    Just (String.fromFloat << CalculatorService.kmToMi)
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> convertationResult

                Yard ->
                    Just (String.fromInt << CalculatorService.kmToYd)
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> convertationResult

        Meter ->
            case to of
                Meter ->
                    ConvertationSkipped

                Kilometer ->
                    Just (String.fromFloat << CalculatorService.mToKm)
                        |> MaybeExtra.andMap (String.toInt value)
                        |> convertationResult

                Mile ->
                    Just (String.fromFloat << CalculatorService.mToMi)
                        |> MaybeExtra.andMap (String.toInt value)
                        |> convertationResult

                Yard ->
                    Just (String.fromInt << CalculatorService.mToYd)
                        |> MaybeExtra.andMap (String.toInt value)
                        |> convertationResult

        Mile ->
            case to of
                Mile ->
                    ConvertationSkipped

                Kilometer ->
                    Just (String.fromFloat << CalculatorService.miToKm)
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> convertationResult

                Meter ->
                    Just (String.fromInt << CalculatorService.miToM)
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> convertationResult

                Yard ->
                    Just (String.fromInt << CalculatorService.miToYd)
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> convertationResult

        Yard ->
            case to of
                Yard ->
                    ConvertationSkipped

                Kilometer ->
                    Just (String.fromFloat << CalculatorService.ydToKm)
                        |> MaybeExtra.andMap (String.toInt value)
                        |> convertationResult

                Meter ->
                    Just (String.fromInt << CalculatorService.ydToM)
                        |> MaybeExtra.andMap (String.toInt value)
                        |> convertationResult

                Mile ->
                    Just (String.fromFloat << CalculatorService.ydToMi)
                        |> MaybeExtra.andMap (String.toInt value)
                        |> convertationResult


convertPace : Pace -> Pace -> String -> Convertation
convertPace from to value =
    case from of
        PerKilometer ->
            case to of
                PerKilometer ->
                    ConvertationSkipped

                PerMile ->
                    Just (ConverterService.secToPace << CalculatorService.secPerKmToSecPerMi)
                        |> MaybeExtra.andMap (ConverterService.paceToSec value)
                        |> convertationResult

        PerMile ->
            case to of
                PerMile ->
                    ConvertationSkipped

                PerKilometer ->
                    Just (ConverterService.secToPace << CalculatorService.secPerMiToSecPerKm)
                        |> MaybeExtra.andMap (ConverterService.paceToSec value)
                        |> convertationResult


convertationResult : Maybe String -> Convertation
convertationResult result =
    result
        |> Maybe.map Converted
        |> Maybe.withDefault ConvertationFailed
