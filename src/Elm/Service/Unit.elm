module Elm.Service.Unit exposing
    ( Convertation(..)
    , Distance(..)
    , Pace(..)
    , Speed(..)
    , Unit(..)
    , convert
    , fromId
    , toId
    )

import Basics.Extra as BasicsExtra
import Elm.Service.Calculator as CalculatorService
import Elm.Service.Converter as ConverterService
import Maybe.Extra as MaybeExtra


type Unit
    = Distance Distance
    | Time
    | Pace Pace
    | Speed Speed


type Distance
    = Kilometer
    | Meter
    | Mile
    | Yard


type Pace
    = PerKilometer
    | PerMile
    | Per100Meters
    | Per100Yards


type Speed
    = KilometersPerHour
    | MilesPerHour


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

        ( Speed unitFrom, Speed unitTo ) ->
            convertSpeed unitFrom unitTo value

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
                    Just CalculatorService.kmToM
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeInteger
                        |> Maybe.map String.fromInt
                        |> convertationResult

                Mile ->
                    Just CalculatorService.kmToMi
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult

                Yard ->
                    Just CalculatorService.kmToYd
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeInteger
                        |> Maybe.map String.fromInt
                        |> convertationResult

        Meter ->
            case to of
                Meter ->
                    ConvertationSkipped

                Kilometer ->
                    Just CalculatorService.mToKm
                        |> MaybeExtra.andMap (String.toInt value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult

                Mile ->
                    Just CalculatorService.mToMi
                        |> MaybeExtra.andMap (String.toInt value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult

                Yard ->
                    Just CalculatorService.mToYd
                        |> MaybeExtra.andMap (String.toInt value)
                        |> isSafeInteger
                        |> Maybe.map String.fromInt
                        |> convertationResult

        Mile ->
            case to of
                Mile ->
                    ConvertationSkipped

                Kilometer ->
                    Just CalculatorService.miToKm
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult

                Meter ->
                    Just CalculatorService.miToM
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeInteger
                        |> Maybe.map String.fromInt
                        |> convertationResult

                Yard ->
                    Just CalculatorService.miToYd
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeInteger
                        |> Maybe.map String.fromInt
                        |> convertationResult

        Yard ->
            case to of
                Yard ->
                    ConvertationSkipped

                Kilometer ->
                    Just CalculatorService.ydToKm
                        |> MaybeExtra.andMap (String.toInt value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult

                Meter ->
                    Just CalculatorService.ydToM
                        |> MaybeExtra.andMap (String.toInt value)
                        |> isSafeInteger
                        |> Maybe.map String.fromInt
                        |> convertationResult

                Mile ->
                    Just CalculatorService.ydToMi
                        |> MaybeExtra.andMap (String.toInt value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult


convertPace : Pace -> Pace -> String -> Convertation
convertPace from to value =
    case from of
        PerKilometer ->
            case to of
                PerKilometer ->
                    ConvertationSkipped

                PerMile ->
                    Just CalculatorService.secPerKmToSecPerMi
                        |> MaybeExtra.andMap (ConverterService.paceToSec value)
                        |> isSafeInteger
                        |> Maybe.map ConverterService.secToPace
                        |> convertationResult

                Per100Meters ->
                    ConvertationSkipped

                Per100Yards ->
                    ConvertationSkipped

        PerMile ->
            case to of
                PerMile ->
                    ConvertationSkipped

                PerKilometer ->
                    Just CalculatorService.secPerMiToSecPerKm
                        |> MaybeExtra.andMap (ConverterService.paceToSec value)
                        |> isSafeInteger
                        |> Maybe.map ConverterService.secToPace
                        |> convertationResult

                Per100Meters ->
                    ConvertationSkipped

                Per100Yards ->
                    ConvertationSkipped

        Per100Meters ->
            case to of
                Per100Meters ->
                    ConvertationSkipped

                Per100Yards ->
                    Just CalculatorService.secPerMToSecPerYd
                        |> MaybeExtra.andMap (ConverterService.paceToSec value)
                        |> isSafeInteger
                        |> Maybe.map ConverterService.secToPace
                        |> convertationResult

                PerKilometer ->
                    ConvertationSkipped

                PerMile ->
                    ConvertationSkipped

        Per100Yards ->
            case to of
                Per100Yards ->
                    ConvertationSkipped

                Per100Meters ->
                    Just CalculatorService.secPerYdToSecPerM
                        |> MaybeExtra.andMap (ConverterService.paceToSec value)
                        |> isSafeInteger
                        |> Maybe.map ConverterService.secToPace
                        |> convertationResult

                PerKilometer ->
                    ConvertationSkipped

                PerMile ->
                    ConvertationSkipped


convertSpeed : Speed -> Speed -> String -> Convertation
convertSpeed from to value =
    case from of
        KilometersPerHour ->
            case to of
                KilometersPerHour ->
                    ConvertationSkipped

                MilesPerHour ->
                    Just CalculatorService.kmPerHToMiPerH
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult

        MilesPerHour ->
            case to of
                MilesPerHour ->
                    ConvertationSkipped

                KilometersPerHour ->
                    Just CalculatorService.miPerHToKmPerH
                        |> MaybeExtra.andMap (String.toFloat value)
                        |> isSafeFloat
                        |> Maybe.map String.fromFloat
                        |> convertationResult


isSafeFloat : Maybe Float -> Maybe Float
isSafeFloat num =
    num
      |> Maybe.map
          (\float ->
              if BasicsExtra.isSafeInteger <| floor float then
                  num

              else
                  Nothing
          )
      |> Maybe.withDefault Nothing



isSafeInteger : Maybe Int -> Maybe Int
isSafeInteger num =
    num
      |> Maybe.map
          (\integer ->
              if BasicsExtra.isSafeInteger integer then
                  num

              else
                  Nothing
          )
      |> Maybe.withDefault Nothing


convertationResult : Maybe String -> Convertation
convertationResult result =
    result
        |> Maybe.map Converted
        |> Maybe.withDefault ConvertationFailed


toId : Unit -> Int
toId unit =
    case unit of
        Distance type_ ->
            case type_ of
                Kilometer ->
                    0

                Meter ->
                    1

                Mile ->
                    2

                Yard ->
                    3

        Time ->
            4

        Pace type_ ->
            case type_ of
                PerKilometer ->
                    5

                PerMile ->
                    6

                Per100Meters ->
                    7

                Per100Yards ->
                    8

        Speed type_ ->
            case type_ of
                KilometersPerHour ->
                    9

                MilesPerHour ->
                    10


fromId : Int -> Unit
fromId id =
    case id of
        0 ->
            Distance Kilometer

        1 ->
            Distance Meter

        2 ->
            Distance Mile

        3 ->
            Distance Yard

        4 ->
            Time

        5 ->
            Pace PerKilometer

        6 ->
            Pace PerMile

        7 ->
            Pace Per100Meters

        8 ->
            Pace Per100Yards

        9 ->
            Speed KilometersPerHour

        10 ->
            Speed MilesPerHour

        _ ->
            Distance Kilometer
