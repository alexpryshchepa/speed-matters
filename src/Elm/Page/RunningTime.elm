module Elm.Page.RunningTime exposing
    ( ExternalMsg(..)
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

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
    , pace : InputElement.Model
    , result : ResultElement.Model
    , isCalculated : Bool
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = DistanceInputMsg InputElement.Msg
    | PaceInputMsg InputElement.Msg
    | ResultMsg ResultElement.Msg
    | CalculateTime
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
    , pace : String
    , paceUnit : Int
    , isCalculated : Bool
    }


db : String
db =
    "running-time"


storageEncoder : Storage -> Encode.Value
storageEncoder storage =
    Encode.object
        [ ( "distance", Encode.string storage.distance )
        , ( "distanceUnit", Encode.int storage.distanceUnit )
        , ( "pace", Encode.string storage.pace )
        , ( "paceUnit", Encode.int storage.paceUnit )
        , ( "isCalculated", Encode.bool storage.isCalculated )
        ]


storageDecoder : Decode.Decoder Storage
storageDecoder =
    Decode.map5 Storage
        (Decode.at [ "distance" ] Decode.string)
        (Decode.at [ "distanceUnit" ] Decode.int)
        (Decode.at [ "pace" ] Decode.string)
        (Decode.at [ "paceUnit" ] Decode.int)
        (Decode.at [ "isCalculated" ] Decode.bool)


init : ( Model, Cmd Msg )
init =
    ( { distance =
            InputElement.init <| UnitService.Distance UnitService.Kilometer
      , pace =
            InputElement.init <| UnitService.Pace UnitService.PerKilometer
      , result =
            ResultElement.init UnitService.Time
      , isCalculated = False
      }
      -- FIXME
    , Process.sleep 100 |> Task.perform (\_ -> Self GetFromLocalStorage)
    )


setInputValue : String -> InputElement.Model -> InputElement.Model
setInputValue value model =
    { model | value = value }


setInputUnit : UnitService.Unit -> InputElement.Model -> InputElement.Model
setInputUnit unit model =
    { model | unit = unit }


setResultValue : String -> ResultElement.Model -> ResultElement.Model
setResultValue value model =
    { model | value = value }


setResultUnit : UnitService.Unit -> ResultElement.Model -> ResultElement.Model
setResultUnit unit model =
    { model | unit = unit }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        clearResult =
            setResultValue "..." model.result
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

        PaceInputMsg (InputElement.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    InputElement.update subMsg model.pace
            in
            ( { model | pace = updatedModel }
            , Cmd.map (Self << PaceInputMsg) cmd
            )

        PaceInputMsg (InputElement.Parent subMsg) ->
            case subMsg of
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

                _ ->
                    ( model
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
                        CmdUtil.fire <| Self CalculateTime

                    else
                        Cmd.none
            )

        CalculateTime ->
            let
                error message =
                    ( { model
                        | result = clearResult
                      }
                    , CmdUtil.fire <| (Parent << ShowSnackbar) message
                    )

                success int =
                    ( { model
                        | isCalculated = True
                        , result =
                            setResultValue
                                (ConverterService.secToTime int)
                                model.result
                      }
                    , Cmd.batch
                        [ CmdUtil.fire (Parent HideSnackbar)
                        , Port.saveToLocalStorage
                            ( db
                            , storageEncoder
                                { distance = model.distance.value
                                , distanceUnit = UnitService.toId model.distance.unit
                                , pace = model.pace.value
                                , paceUnit = UnitService.toId model.pace.unit
                                , isCalculated = True
                                }
                            )
                        ]
                    )

                validation =
                    validate model.distance model.pace
            in
            case validation of
                ValidationError message ->
                    error message

                ValidationSuccess ->
                    let
                        calculation =
                            calculate model.distance model.pace
                    in
                    case calculation of
                        CalculationSuccess sec ->
                            success sec

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | distance = setInputValue "" model.distance
                , pace = setInputValue "" model.pace
                , result = setResultValue "..." model.result
                , isCalculated = False
              }
            , Port.saveToLocalStorage
                ( db
                , storageEncoder
                    { distance = ""
                    , distanceUnit = UnitService.toId model.distance.unit
                    , pace = ""
                    , paceUnit = UnitService.toId model.pace.unit
                    , isCalculated = False
                    }
                )
            )

        LocalStorageResponse value ->
            let
                decodeStorage =
                    Decode.decodeValue storageDecoder value

                { distance, pace, result } =
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

                        updatedPaceInput =
                            { pace
                                | value = storage.pace
                                , unit = UnitService.fromId storage.paceUnit
                            }
                    in
                    ( { model
                        | distance = updatedDistanceInput
                        , pace = updatedPaceInput
                      }
                    , if storage.isCalculated then
                        CmdUtil.fire <| Self CalculateTime

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
validate distance pace =
    case ( distance.isValid, pace.isValid ) of
        ( False, False ) ->
            ValidationError "Both fields are not valid, please take a look at the hints"

        ( True, False ) ->
            ValidationError "Not valid value for pace"

        ( False, True ) ->
            ValidationError "Not valid value for distance"

        ( True, True ) ->
            -- Fields are valid but can be empty, so check emptyness next
            case ( not <| String.isEmpty distance.value, not <| String.isEmpty pace.value ) of
                ( False, False ) ->
                    ValidationError "Both fields are empty. Please fill the fields to calculate time"

                ( True, False ) ->
                    ValidationError "Please add pace value"

                ( False, True ) ->
                    ValidationError "Please add distance value"

                ( True, True ) ->
                    -- Fields are valid and have value, so we can try to calculate pace
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> Calculation
calculate distance pace =
    let
        error =
            CalculationError "Something went wrong. Please contact us"
    in
    case ( String.toFloat distance.value, ConverterService.paceToSec pace.value ) of
        ( Just d, Just p ) ->
            case ( distance.unit, pace.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time d p

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.mToKm (truncate d)) p

                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.kmToMi d) p

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.mToMi (truncate d)) p

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time d p

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.miToKm d) p

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.ydToMi (truncate d)) p

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.ydToKm (truncate d)) p

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
                        { name = "Running distance"
                        , units =
                            ( "running-pace-distance"
                            , [ { unit = UnitService.Distance UnitService.Kilometer
                                , name = "Kilometers"
                                , hint = "Must be a number, e.g 21.098"
                                , shortcut = "km"
                                , regex = ValidatorService.floatRegex
                                , error = "Wrong value, please make sure you value is float or integer number e.g. 42.195"
                                }
                              , { unit = UnitService.Distance UnitService.Meter
                                , name = "Meters"
                                , hint = "Must be a number, e.g 5000"
                                , shortcut = "m"
                                , regex = ValidatorService.intRegex
                                , error = "Wrong value, please make sure you value is integer number e.g. 10000"
                                }
                              , { unit = UnitService.Distance UnitService.Mile
                                , name = "Miles"
                                , hint = "Must be a number, e.g 26.1"
                                , shortcut = "mi"
                                , regex = ValidatorService.floatRegex
                                , error = "Wrong value, please make sure you value is float or integer number e.g. 42.195"
                                }
                              , { unit = UnitService.Distance UnitService.Yard
                                , name = "Yards"
                                , hint = "Must be a number, e.g 1000"
                                , shortcut = "yd"
                                , regex = ValidatorService.intRegex
                                , error = "Wrong value, please make sure you value is integer number e.g. 10000"
                                }
                              ]
                            )
                        , links =
                            [ { name = "Half Marathon"
                              , value = "21.098"
                              , unit = UnitService.Distance UnitService.Kilometer
                              }
                            , { name = "Marathon"
                              , value = "42195"
                              , unit = UnitService.Distance UnitService.Meter
                              }
                            ]
                        }
                        model.distance
                , Html.map (Self << PaceInputMsg) <|
                    InputElement.view
                        { name = "Running pace"
                        , units =
                            ( ""
                            , [ { unit = UnitService.Pace UnitService.PerKilometer
                                , name = "Per kilometer"
                                , hint = "You should follow this pattern - MM:SS"
                                , shortcut = "min/km"
                                , regex = ValidatorService.paceRegex
                                , error = "Wrong value, please make sure you added leading zeros and do not overreach time format (max: 59:59)"
                                }
                              , { unit = UnitService.Pace UnitService.PerMile
                                , name = "Per mile"
                                , hint = "You should follow this pattern - MM:SS"
                                , shortcut = "min/mi"
                                , regex = ValidatorService.paceRegex
                                , error = "Wrong value, please make sure you added leading zeros and do not overreach time format (max: 59:59)"
                                }
                              ]
                            )
                        , links =
                            []
                        }
                        model.pace
                ]

        result =
            div []
                [ Html.map (Self << ResultMsg) <|
                    ResultElement.view
                        { title = "Your time is"
                        , units =
                            ( "running-time-result"
                            , []
                            )
                        }
                        model.result
                ]

        description =
            div []
                [ h2 [ class "mdc-typography mdc-typography--headline4" ] [ text "Who Uses a Running Time Calculator?" ]
                ]
    in
    PageLayout.view
        { form = form
        , result = result
        , calculate =
            { msg = Self CalculateTime
            , isCalculated = model.isCalculated
            }
        , reset = Self ResetForm
        , description = description
        }
