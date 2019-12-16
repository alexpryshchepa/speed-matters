module Elm.Page.SwimmingDistance exposing
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
    { time : InputElement.Model
    , pace : InputElement.Model
    , result : ResultElement.Model
    , isCalculated : Bool
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = TimeInputMsg InputElement.Msg
    | PaceInputMsg InputElement.Msg
    | ResultMsg ResultElement.Msg
    | CalculateDistance
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
    | CalculationSuccess Float


type alias Storage =
    { time : String
    , pace : String
    , paceUnit : Int
    , distanceUnit : Int
    , isCalculated : Bool
    }


db : String
db =
    "swimming-distance"


storageEncoder : Storage -> Encode.Value
storageEncoder storage =
    Encode.object
        [ ( "time", Encode.string storage.time )
        , ( "pace", Encode.string storage.pace )
        , ( "paceUnit", Encode.int storage.paceUnit )
        , ( "distanceUnit", Encode.int storage.distanceUnit )
        , ( "isCalculated", Encode.bool storage.isCalculated )
        ]


storageDecoder : Decode.Decoder Storage
storageDecoder =
    Decode.map5 Storage
        (Decode.at [ "time" ] Decode.string)
        (Decode.at [ "pace" ] Decode.string)
        (Decode.at [ "paceUnit" ] Decode.int)
        (Decode.at [ "distanceUnit" ] Decode.int)
        (Decode.at [ "isCalculated" ] Decode.bool)


init : ( Model, Cmd Msg )
init =
    ( { time =
            InputElement.init UnitService.Time
      , pace =
            InputElement.init <| UnitService.Pace UnitService.Per100Meters
      , result =
            ResultElement.init <| UnitService.Distance UnitService.Meter
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
                        CmdUtil.fire <| Self CalculateDistance

                    else
                        Cmd.none
            )

        CalculateDistance ->
            let
                error message =
                    ( { model
                        | result = clearResult
                        , isCalculated = False
                      }
                    , CmdUtil.fire <| (Parent << ShowSnackbar) message
                    )

                success float =
                    ( { model
                        | isCalculated = True
                        , result =
                            ResultElement.setValue
                                (String.fromFloat float)
                                model.result
                      }
                    , Cmd.batch
                        [ CmdUtil.fire (Parent HideSnackbar)
                        , Port.saveToLocalStorage
                            ( db
                            , storageEncoder
                                { time = model.time.value
                                , pace = model.pace.value
                                , paceUnit = UnitService.toId model.pace.unit
                                , distanceUnit = UnitService.toId model.result.unit
                                , isCalculated = True
                                }
                            )
                        ]
                    )

                validation =
                    validate model.time model.pace
            in
            case validation of
                ValidationError message ->
                    error message

                ValidationSuccess ->
                    let
                        calculation =
                            calculate model.time model.pace model.result
                    in
                    case calculation of
                        CalculationSuccess distance ->
                            if BasicsExtra.isSafeInteger <| floor distance then
                                success distance

                            else
                                error "The distance is out of range, please insert smaller values of time or pace"

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | time = InputElement.setValue "" model.time
                , pace = InputElement.setValue "" model.pace
                , result = ResultElement.setValue "..." model.result
                , isCalculated = False
              }
            , Port.saveToLocalStorage
                ( db
                , storageEncoder
                    { time = ""
                    , pace = ""
                    , paceUnit = UnitService.toId model.pace.unit
                    , distanceUnit = UnitService.toId model.result.unit
                    , isCalculated = False
                    }
                )
            )

        LocalStorageResponse value ->
            let
                decodeStorage =
                    Decode.decodeValue storageDecoder value

                { time, pace, result } =
                    model
            in
            case decodeStorage of
                Ok storage ->
                    let
                        updatedTimeInput =
                            { time
                                | value = storage.time
                            }

                        updatedPaceInput =
                            { pace
                                | value = storage.pace
                                , unit = UnitService.fromId storage.paceUnit
                            }
                    in
                    ( { model
                        | time = updatedTimeInput
                        , pace = updatedPaceInput
                      }
                    , if storage.isCalculated then
                        CmdUtil.fire <| Self CalculateDistance

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
validate time pace =
    case ( time.isValid, pace.isValid ) of
        ( False, False ) ->
            ValidationError "Both fields are not valid, please take a look at the hints"

        ( True, False ) ->
            ValidationError "Not valid value for pace"

        ( False, True ) ->
            ValidationError "Not valid value for time"

        ( True, True ) ->
            -- Fields are valid but can be empty, so check emptyness next
            case ( not <| String.isEmpty time.value, not <| String.isEmpty pace.value ) of
                ( False, False ) ->
                    ValidationError "Both fields are empty. Please fill the fields to calculate distance"

                ( True, False ) ->
                    ValidationError "Please add pace value"

                ( False, True ) ->
                    ValidationError "Please add time value"

                ( True, True ) ->
                    -- Fields are valid and have value, so we can try to calculate pace
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> ResultElement.Model -> Calculation
calculate time pace result =
    let
        error =
            CalculationError "Something went wrong. Please contact us"
    in
    case ( ConverterService.timeToSec time.value, ConverterService.paceToSec pace.value ) of
        ( Just t, Just p ) ->
            case ( result.unit, pace.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| CalculatorService.mToKm (round (CalculatorService.distance t p * 100))

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| CalculatorService.roundTo 1000 (CalculatorService.distance t p * 100)

                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| CalculatorService.ydToKm (round (CalculatorService.distance t p * 100))

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| toFloat (CalculatorService.ydToM (round (CalculatorService.distance t p * 100)))

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| CalculatorService.ydToMi (round (CalculatorService.distance t p * 100))

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| CalculatorService.mToMi (round (CalculatorService.distance t p * 100))

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.Per100Yards ) ->
                    CalculationSuccess <| CalculatorService.roundTo 1000 (CalculatorService.distance t p * 100)

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.Per100Meters ) ->
                    CalculationSuccess <| toFloat (CalculatorService.mToYd (round (CalculatorService.distance t p * 100)))

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
                [ Html.map (Self << TimeInputMsg) <|
                    InputElement.view
                        { name = "Swimming time"
                        , units =
                            ( ""
                            , [ { unit = UnitService.Time
                                , name = ""
                                , hint = "You should follow this pattern - HH:MM:SS"
                                , shortcut = ""
                                , regex = ValidatorService.timeRegex
                                , error = "Wrong value, please make sure you added leading zeros and followe HH:MM:SS (hours:minutes:seconds) pattern"
                                }
                              ]
                            )
                        , links =
                            []
                        }
                        model.time
                , Html.map (Self << PaceInputMsg) <|
                    InputElement.view
                        { name = "Swimming pace"
                        , units =
                            ( ""
                            , [ { unit = UnitService.Pace UnitService.Per100Meters
                                , name = "Per meters"
                                , hint = "You should follow this pattern - MM:SS"
                                , shortcut = "min/100m"
                                , regex = ValidatorService.paceRegex
                                , error = "Wrong value, please make sure you added leading zeros and followe MM:SS (minutes:seconds) pattern"
                                }
                              , { unit = UnitService.Pace UnitService.Per100Yards
                                , name = "Per yards"
                                , hint = "You should follow this pattern - MM:SS"
                                , shortcut = "min/100yd"
                                , regex = ValidatorService.paceRegex
                                , error = "Wrong value, please make sure you added leading zeros and followe MM:SS (minutes:seconds) pattern"
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
                        { title = "Your distance is"
                        , units =
                            ( "running-distance-result"
                            , [ { name = "Meters"
                                , unit = UnitService.Distance UnitService.Meter
                                , shortcut = "m"
                                }
                              , { name = "Yards"
                                , unit = UnitService.Distance UnitService.Yard
                                , shortcut = "yd"
                                }
                              , { name = "Kilometers"
                                , unit = UnitService.Distance UnitService.Kilometer
                                , shortcut = "km"
                                }
                              , { name = "Miles"
                                , unit = UnitService.Distance UnitService.Mile
                                , shortcut = "mi"
                                }
                              ]
                            )
                        }
                        model.result
                ]

        description =
            div []
                [ h2 [ class "mdc-typography mdc-typography--headline4" ] [ text "Who Uses a Swimming Distance Calculator?" ]
                ]
    in
    PageLayout.view
        { form = form
        , result = result
        , calculate =
            { msg = Self CalculateDistance
            , isCalculated = model.isCalculated
            }
        , reset = Self ResetForm
        , description = description
        }
