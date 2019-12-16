module Elm.Page.SwimmingPace exposing
    ( ExternalMsg(..)
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Basics.Extra as BasicsExtra
import Elm.Element.Input as InputElement
import Elm.Element.Result as ResultElement
import Elm.Layout.Page as PageLayout
import Elm.Port as Port
import Elm.Service.Calculator as CalculatorService
import Elm.Service.Converter as ConverterService
import Elm.Service.Unit as UnitService
import Elm.Service.Validator as ValidatorService
import Elm.Util.Cmd as CmdUtil
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task


type alias Model =
    { distance : InputElement.Model
    , time : InputElement.Model
    , result : ResultElement.Model
    , isCalculated : Bool
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = DistanceInputMsg InputElement.Msg
    | TimeInputMsg InputElement.Msg
    | ResultMsg ResultElement.Msg
    | CalculatePace
    | ResetForm
    | LocalStorageResponse Encode.Value
    | GetFromLocalStorage


type ExternalMsg
    = ShowSnackbar String
    | HideSnackbar


type Validation
    = ValidationError String
    | ValidationSuccess


type Calculation
    = CalculationError String
    | CalculationSuccess Int


type alias Storage =
    { distance : String
    , distanceUnit : Int
    , time : String
    , resultUnit : Int
    , isCalculated : Bool
    }


db : String
db =
    "swimming-pace"


storageEncoder : Storage -> Encode.Value
storageEncoder storage =
    Encode.object
        [ ( "distance", Encode.string storage.distance )
        , ( "distanceUnit", Encode.int storage.distanceUnit )
        , ( "time", Encode.string storage.time )
        , ( "resultUnit", Encode.int storage.resultUnit )
        , ( "isCalculated", Encode.bool storage.isCalculated )
        ]


storageDecoder : Decode.Decoder Storage
storageDecoder =
    Decode.map5 Storage
        (Decode.at [ "distance" ] Decode.string)
        (Decode.at [ "distanceUnit" ] Decode.int)
        (Decode.at [ "time" ] Decode.string)
        (Decode.at [ "resultUnit" ] Decode.int)
        (Decode.at [ "isCalculated" ] Decode.bool)


init : ( Model, Cmd Msg )
init =
    ( { distance =
            InputElement.init <| UnitService.Distance UnitService.Meter
      , time =
            InputElement.init UnitService.Time
      , result =
            ResultElement.init <| UnitService.Pace UnitService.Per100Meters
      , isCalculated = False
      }
      -- FIXME
    , Process.sleep 100 |> Task.perform (\_ -> Self GetFromLocalStorage)
    )


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        clearResult =
            ResultElement.setValue "..." model.result
    in
    case msg of
        DistanceInputMsg (InputElement.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    InputElement.update subMsg model.distance
            in
            ( { model | distance = updatedModel }
            , Cmd.map (Self << DistanceInputMsg) cmd
            )

        DistanceInputMsg (InputElement.Parent subMsg) ->
            case subMsg of
                InputElement.ShowSnackbar message ->
                    ( model
                    , CmdUtil.fire <| (Parent << ShowSnackbar) message
                    )

                InputElement.ValueChanged old new ->
                    if model.isCalculated then
                        ( { model
                            | isCalculated = False
                            , result = clearResult
                          }
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                InputElement.ConvertationFailed ->
                    ( { model | result = ResultElement.setValue "" model.result }
                    , Cmd.none
                    )

        TimeInputMsg (InputElement.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    InputElement.update subMsg model.time
            in
            ( { model | time = updatedModel }
            , Cmd.map (Self << TimeInputMsg) cmd
            )

        TimeInputMsg (InputElement.Parent subMsg) ->
            case subMsg of
                InputElement.ShowSnackbar message ->
                    ( model
                    , CmdUtil.fire <| (Parent << ShowSnackbar) message
                    )

                InputElement.ValueChanged old new ->
                    if model.isCalculated then
                        ( { model
                            | isCalculated = False
                            , result = clearResult
                          }
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                InputElement.ConvertationFailed ->
                    ( { model | result = ResultElement.setValue "" model.result }
                    , Cmd.none
                    )

        ResultMsg (ResultElement.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    ResultElement.update subMsg model.result
            in
            ( { model | result = updatedModel }
            , Cmd.map (Self << ResultMsg) cmd
            )

        ResultMsg (ResultElement.Parent subMsg) ->
            ( model
            , case subMsg of
                ResultElement.UnitChanged ->
                    if model.isCalculated then
                        CmdUtil.fire <| Self CalculatePace

                    else
                        Cmd.none
            )

        CalculatePace ->
            let
                error message =
                    ( { model
                        | result = clearResult
                        , isCalculated = False
                      }
                    , CmdUtil.fire <| (Parent << ShowSnackbar) message
                    )

                success int =
                    ( { model
                        | isCalculated = True
                        , result =
                            ResultElement.setValue
                                (ConverterService.secToPace int)
                                model.result
                      }
                    , Cmd.batch
                        [ CmdUtil.fire (Parent HideSnackbar)
                        , Port.saveToLocalStorage
                            ( db
                            , storageEncoder
                                { distance = model.distance.value
                                , distanceUnit = UnitService.toId model.distance.unit
                                , time = model.time.value
                                , resultUnit = UnitService.toId model.result.unit
                                , isCalculated = True
                                }
                            )
                        ]
                    )

                validation =
                    validate model.distance model.time
            in
            case validation of
                ValidationError message ->
                    error message

                ValidationSuccess ->
                    let
                        calculation =
                            calculate model.distance model.time model.result
                    in
                    case calculation of
                        CalculationSuccess sec ->
                            if BasicsExtra.isSafeInteger sec then
                                success sec

                            else
                                error "The pace is out of range, please insert smaller values of distance or time"

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | distance = InputElement.setValue "" model.distance
                , time = InputElement.setValue "" model.time
                , result = ResultElement.setValue "..." model.result
                , isCalculated = False
              }
            , Port.saveToLocalStorage
                ( db
                , storageEncoder
                    { distance = ""
                    , distanceUnit = UnitService.toId model.distance.unit
                    , time = ""
                    , resultUnit = UnitService.toId model.result.unit
                    , isCalculated = False
                    }
                )
            )

        LocalStorageResponse value ->
            let
                decodeStorage =
                    Decode.decodeValue storageDecoder value

                { distance, time, result } =
                    model
            in
            case decodeStorage of
                Ok storage ->
                    let
                        updatedDistanceInput =
                            { distance
                                | value = storage.distance
                                , unit = UnitService.fromId storage.distanceUnit
                            }

                        updatedTimeInput =
                            { time | value = storage.time }

                        updatedResult =
                            { result | unit = UnitService.fromId storage.resultUnit }
                    in
                    ( { model
                        | distance = updatedDistanceInput
                        , time = updatedTimeInput
                        , result = updatedResult
                      }
                    , if storage.isCalculated then
                        CmdUtil.fire <| Self CalculatePace

                      else
                        Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        GetFromLocalStorage ->
            ( model
            , Port.getFromLocalStorage db
            )


validate : InputElement.Model -> InputElement.Model -> Validation
validate distance time =
    case ( distance.isValid, time.isValid ) of
        ( False, False ) ->
            ValidationError "Both fields are not valid, please take a look at the hints"

        ( True, False ) ->
            ValidationError "Not valid value for time"

        ( False, True ) ->
            ValidationError "Not valid value for distance"

        ( True, True ) ->
            -- Fields are valid but can be empty, so check emptyness next
            case ( not <| String.isEmpty distance.value, not <| String.isEmpty time.value ) of
                ( False, False ) ->
                    ValidationError "Both fields are empty. Please fill the fields to calculate pace"

                ( True, False ) ->
                    ValidationError "Please add time value"

                ( False, True ) ->
                    ValidationError "Please add distance value"

                ( True, True ) ->
                    -- Fields are valid and have value, so we can try to calculate pace
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> ResultElement.Model -> Calculation
calculate distance time result =
    let
        error =
            CalculationError "Something went wrong. Please contact us"
    in
    case ( String.toFloat distance.value, ConverterService.timeToSec time.value ) of
        ( Just d, Just t ) ->
            case ( distance.unit, result.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| CalculatorService.pace t (toFloat (CalculatorService.kmToM d) / 100)

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| CalculatorService.pace t (d / 100)

                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| CalculatorService.pace t (toFloat (CalculatorService.kmToYd d) / 100)

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| CalculatorService.pace t (toFloat (CalculatorService.mToYd (round d)) / 100)

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| CalculatorService.pace t (toFloat (CalculatorService.miToYd d) / 100)

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| CalculatorService.pace t (toFloat (CalculatorService.miToM d) / 100)

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| CalculatorService.pace t (d / 100)

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| CalculatorService.pace t (toFloat (CalculatorService.ydToM (round d)) / 100)

                _ ->
                    error

        _ ->
            error


subscriptions : Model -> Sub Msg
subscriptions model =
    Port.responseFromLocalStorage (Self << LocalStorageResponse)


view : Model -> Html Msg
view model =
    let
        form =
            div []
                [ Html.map (Self << DistanceInputMsg) <|
                    InputElement.view
                        { name = "Swimming distance"
                        , units =
                            ( "swimming-pace-distance"
                            , [ { unit = UnitService.Distance UnitService.Meter
                                , name = "Meters"
                                , hint = "Must be a number, e.g 5000"
                                , shortcut = "m"
                                , regex = ValidatorService.intRegex
                                , error = "Wrong value, please make sure you value is integer number e.g. 10000"
                                }
                              , { unit = UnitService.Distance UnitService.Yard
                                , name = "Yards"
                                , hint = "Must be a number, e.g 1000"
                                , shortcut = "yd"
                                , regex = ValidatorService.intRegex
                                , error = "Wrong value, please make sure you value is integer number e.g. 10000"
                                }
                              , { unit = UnitService.Distance UnitService.Kilometer
                                , name = "Kilometers"
                                , hint = "Must be a number, e.g 21.098"
                                , shortcut = "km"
                                , regex = ValidatorService.floatRegex
                                , error = "Wrong value, please make sure you value is float or integer number e.g. 42.195"
                                }
                              , { unit = UnitService.Distance UnitService.Mile
                                , name = "Miles"
                                , hint = "Must be a number, e.g 26.1"
                                , shortcut = "mi"
                                , regex = ValidatorService.floatRegex
                                , error = "Wrong value, please make sure you value is float or integer number e.g. 42.195"
                                }
                              ]
                            )
                        , links =
                            [ { name = "Bosphorus"
                              , value = "6.5"
                              , unit = UnitService.Distance UnitService.Kilometer
                              }
                            , { name = "IM 70.3"
                              , value = "1900"
                              , unit = UnitService.Distance UnitService.Meter
                              }
                            , { name = "IM 140.6"
                              , value = "3800"
                              , unit = UnitService.Distance UnitService.Meter
                              }
                            ]
                        }
                        model.distance
                , Html.map (Self << TimeInputMsg) <|
                    InputElement.view
                        { name = "Swimming time"
                        , units =
                            ( "swimming-pace-time"
                            , [ { unit = UnitService.Time
                                , name = ""
                                , hint = "You should follow this pattern - HH:MM:SS"
                                , shortcut = ""
                                , regex = ValidatorService.timeRegex
                                , error = "Wrong value, please make sure you added leading zeros and followe HH:MM:SS (hours:minutes:seconds) pattern"
                                }
                              ]
                            )
                        , links = []
                        }
                        model.time
                ]

        result =
            div []
                [ Html.map (Self << ResultMsg) <|
                    ResultElement.view
                        { title = "Your pace is"
                        , units =
                            ( "swimming-pace-result"
                            , [ { name = "Per meters"
                                , unit = UnitService.Pace UnitService.Per100Meters
                                , shortcut = "min/100m"
                                }
                              , { name = "Per yards"
                                , unit = UnitService.Pace UnitService.Per100Yards
                                , shortcut = "min/100yd"
                                }
                              ]
                            )
                        }
                        model.result
                ]

        description =
            div []
                [ h2 [ class "mdc-typography mdc-typography--headline4" ] [ text "Who Uses a Swimming Pace Calculator?" ]
                ]
    in
    PageLayout.view
        { form = form
        , result = result
        , calculate =
            { msg = Self CalculatePace
            , isCalculated = model.isCalculated
            }
        , reset = Self ResetForm
        , description = description
        }
