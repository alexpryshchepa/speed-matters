module Elm.Page.RunningTime exposing
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


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
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

                InputElement.ValueChanged _ _ ->
                    if model.isCalculated then
                        ( { model
                            | isCalculated = False
                            , result = ResultElement.clearValue model.result
                          }
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                InputElement.ConvertationFailed ->
                    ( { model | result = ResultElement.clearValue model.result }
                    , Cmd.none
                    )

                InputElement.UnitChanged ->
                    ( model
                    , if model.isCalculated then
                        CmdUtil.fire <| Self CalculateTime

                      else
                        Cmd.none
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

                InputElement.ValueChanged _ _ ->
                    if model.isCalculated then
                        ( { model
                            | isCalculated = False
                            , result = ResultElement.clearValue model.result
                          }
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                InputElement.ConvertationFailed ->
                    ( { model | result = ResultElement.clearValue model.result }
                    , Cmd.none
                    )

                InputElement.UnitChanged ->
                    ( model
                    , if model.isCalculated then
                        CmdUtil.fire <| Self CalculateTime

                      else
                        Cmd.none
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
                        | result = ResultElement.clearValue model.result
                        , isCalculated = False
                      }
                    , CmdUtil.fire <| (Parent << ShowSnackbar) message
                    )

                success int =
                    ( { model
                        | isCalculated = True
                        , result =
                            ResultElement.setValue
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
                            if BasicsExtra.isSafeInteger sec then
                                success sec

                            else
                                error <| ValidatorService.outOfRange "time"

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | distance = InputElement.setValue "" model.distance
                , pace = InputElement.setValue "" model.pace
                , result = ResultElement.clearValue model.result
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

                { distance, pace } =
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
            ValidationError ValidatorService.fieldsNotValid

        ( False, True ) ->
            ValidationError <| ValidatorService.fieldNotValid "distance"

        ( True, False ) ->
            ValidationError <| ValidatorService.fieldNotValid "pace"

        ( True, True ) ->
            case ( not <| String.isEmpty distance.value, not <| String.isEmpty pace.value ) of
                ( False, False ) ->
                    ValidationError <| ValidatorService.fieldsEmpty "time"

                ( False, True ) ->
                    ValidationError <| ValidatorService.fieldEmpty "distance"

                ( True, False ) ->
                    ValidationError <| ValidatorService.fieldEmpty "pace"

                ( True, True ) ->
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> Calculation
calculate distance pace =
    case ( String.toFloat distance.value, ConverterService.paceToSec pace.value ) of
        ( Just d, Just p ) ->
            case ( distance.unit, pace.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time d p

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.mToKm (round d)) p

                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.kmToMi d) p

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.mToMi (round d)) p

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time d p

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.miToKm d) p

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.ydToMi (round d)) p

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.time (CalculatorService.ydToKm (round d)) p

                _ ->
                    CalculationError ValidatorService.unhandledException

        _ ->
            CalculationError ValidatorService.unhandledException


subscriptions : Model -> Sub Msg
subscriptions _ =
    Port.responseFromLocalStorage (Self << LocalStorageResponse)


view : Model -> Html Msg
view model =
    let
        form =
            [ Html.map (Self << DistanceInputMsg) <|
                InputElement.view
                    { name = "Running distance"
                    , units =
                        ( "running-time-distance"
                        , [ { unit = UnitService.Distance UnitService.Kilometer
                            , name = "Kilometers"
                            , hint = ValidatorService.floatHint
                            , shortcut = "km"
                            , regex = ValidatorService.floatRegex
                            , error = ValidatorService.floatError
                            }
                          , { unit = UnitService.Distance UnitService.Meter
                            , name = "Meters"
                            , hint = ValidatorService.integerHint
                            , shortcut = "m"
                            , regex = ValidatorService.intRegex
                            , error = ValidatorService.integerError
                            }
                          , { unit = UnitService.Distance UnitService.Mile
                            , name = "Miles"
                            , hint = ValidatorService.floatHint
                            , shortcut = "mi"
                            , regex = ValidatorService.floatRegex
                            , error = ValidatorService.floatError
                            }
                          , { unit = UnitService.Distance UnitService.Yard
                            , name = "Yards"
                            , hint = ValidatorService.integerHint
                            , shortcut = "yd"
                            , regex = ValidatorService.intRegex
                            , error = ValidatorService.integerError
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
                        ( "running-time-pace"
                        , [ { unit = UnitService.Pace UnitService.PerKilometer
                            , name = "Per kilometer"
                            , hint = ValidatorService.paceHint
                            , shortcut = "min/km"
                            , regex = ValidatorService.paceRegex
                            , error = ValidatorService.paceError
                            }
                          , { unit = UnitService.Pace UnitService.PerMile
                            , name = "Per mile"
                            , hint = ValidatorService.paceHint
                            , shortcut = "min/mi"
                            , regex = ValidatorService.paceRegex
                            , error = ValidatorService.paceError
                            }
                          ]
                        )
                    , links =
                        []
                    }
                    model.pace
            ]

        result =
            [ Html.map (Self << ResultMsg) <|
                ResultElement.view
                    { title = "Time is"
                    , units =
                        ( ""
                        , []
                        )
                    }
                    model.result
            ]

        description =
            [ h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "How to calculate running time?" ]
            , p [ class "mdc-typography mdc-typography--body1" ]
                [ text
                    """
                    To calculate time you need the pace you ran at and the distance of your training run or race.
                    The world`s average time to complete a marathon is around 5 hours.
                    Most men finish a marathon in under 4 and a half hours. Most women finish in just under 5 hours.
                    Elite runners can run the distance in about 2 hours.
                    """
                ]
            , h1 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Try to use running time calculator to" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Find out if you can run 10 miles in less than an hour
                        if you will run at the pace of your last hard training session.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Determine the duration of your next training session.
                        """
                    ]
                ]
            , h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Interesting facts" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Dean Karnazes is an extraordinary athlete with high endurance.
                        One of his greatest achievements is running for 80 hours and 44 minutes
                        without sleep in 2005. During the race, he run  560 km (350 mi).
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Japanese Nao Kazami is one of the outstanding personalities in running.
                        On June 24, 2018, he broke a 100 km running record with the
                        result of 6 hours 9 minutes 14 seconds.
                        """
                    ]
                ]
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
