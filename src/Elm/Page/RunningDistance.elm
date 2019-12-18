module Elm.Page.RunningDistance exposing
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
    "running-distance"


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
            InputElement.init <| UnitService.Pace UnitService.PerKilometer
      , result =
            ResultElement.init <| UnitService.Distance UnitService.Kilometer
      , isCalculated = False
      }
      -- FIXME
    , Process.sleep 100 |> Task.perform (\_ -> Self GetFromLocalStorage)
    )


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                        CmdUtil.fire <| Self CalculateDistance

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

                InputElement.ValueChanged old new ->
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
                        CmdUtil.fire <| Self CalculateDistance

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
                        CmdUtil.fire <| Self CalculateDistance

                    else
                        Cmd.none
            )

        CalculateDistance ->
            let
                error message =
                    ( { model
                        | result = ResultElement.clearValue model.result
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
                                error <| ValidatorService.outOfRange "distance"

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | time = InputElement.setValue "" model.time
                , pace = InputElement.setValue "" model.pace
                , result = ResultElement.clearValue model.result
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

                        updatedResultInput =
                            { result | unit = UnitService.fromId storage.distanceUnit }
                    in
                    ( { model
                        | time = updatedTimeInput
                        , pace = updatedPaceInput
                        , result = updatedResultInput
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
            ValidationError ValidatorService.fieldsNotValid

        ( False, True ) ->
            ValidationError <| ValidatorService.fieldNotValid "time"

        ( True, False ) ->
            ValidationError <| ValidatorService.fieldNotValid "pace"

        ( True, True ) ->
            case ( not <| String.isEmpty time.value, not <| String.isEmpty pace.value ) of
                ( False, False ) ->
                    ValidationError <| ValidatorService.fieldsEmpty "distance"

                ( False, True ) ->
                    ValidationError <| ValidatorService.fieldEmpty "time"

                ( True, False ) ->
                    ValidationError <| ValidatorService.fieldEmpty "pace"

                ( True, True ) ->
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> ResultElement.Model -> Calculation
calculate time pace result =
    case ( ConverterService.timeToSec time.value, ConverterService.paceToSec pace.value ) of
        ( Just t, Just p ) ->
            case ( result.unit, pace.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.distance t p

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| toFloat (CalculatorService.kmToM (CalculatorService.distance t p))

                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.miToKm (CalculatorService.distance t p)

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| toFloat (CalculatorService.miToM (CalculatorService.distance t p))

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.distance t p

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.kmToMi (CalculatorService.distance t p)

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| toFloat (CalculatorService.miToYd (CalculatorService.distance t p))

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| toFloat (CalculatorService.kmToYd (CalculatorService.distance t p))

                _ ->
                    CalculationError ValidatorService.unhandledException

        _ ->
            CalculationError ValidatorService.unhandledException


subscriptions : Model -> Sub Msg
subscriptions model =
    Port.responseFromLocalStorage (Self << LocalStorageResponse)


view : Model -> Html Msg
view model =
    let
        form =
            [ Html.map (Self << TimeInputMsg) <|
                InputElement.view
                    { name = "Running time"
                    , units =
                        ( "running-distance-time"
                        , [ { unit = UnitService.Time
                            , name = ""
                            , hint = ValidatorService.timeHint
                            , shortcut = ""
                            , regex = ValidatorService.timeRegex
                            , error = ValidatorService.timeError
                            }
                          ]
                        )
                    , links =
                        []
                    }
                    model.time
            , Html.map (Self << PaceInputMsg) <|
                InputElement.view
                    { name = "Running pace"
                    , units =
                        ( "running-distance-pace"
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
                    { title = "Distance is"
                    , units =
                        ( "running-distance-result"
                        , [ { name = "Kilometers"
                            , unit = UnitService.Distance UnitService.Kilometer
                            , shortcut = "km"
                            }
                          , { name = "Meters"
                            , unit = UnitService.Distance UnitService.Meter
                            , shortcut = "m"
                            }
                          , { name = "Miles"
                            , unit = UnitService.Distance UnitService.Mile
                            , shortcut = "mi"
                            }
                          , { name = "Yards"
                            , unit = UnitService.Distance UnitService.Yard
                            , shortcut = "yd"
                            }
                          ]
                        )
                    }
                    model.result
            ]

        description =
            [ h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "What do you need to calculate running distance?" ]
            , p [ class "mdc-typography mdc-typography--body1" ]
                [ text
                    """
                    To calculate the distance you need the pace you ran at and the duration of your training run or race.
                    """
                ]
            , h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Some examples" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Determine how far you can run at a certain pace.
                        For example, find out will you reach marathon distance
                        if your pace will be 5:00 per minute throughout 4 hours.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Find out what distance you need to run in your next training session
                        in order to choose a suitable route in advance.
                        """
                    ]
                ]
            , h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Interesting facts" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The Self-Transcendence 3100 Mile Race is one of the most extraordinary running events ever.
                        With its 4,989 km it’s the world’s longest running route –
                        and it leads around just one block in New York.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        A 24-hour run is a form of ultramarathon, in which a competitor runs as far as they can in 24 hours.
                        Top runners will often run 200 kilometres (124 mi) or more,
                        depending on conditions, and the best can go beyond 270 kilometres (168 mi).
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The fastest 100 km ultra distance by a male athlete as sanctioned by the IAU
                        (International Association of Ultrarunners) is 6 hours 9 minutes 14 seconds,
                        achieved by Nao Kazami (Japan) at the Lake Saroma
                        100 km Ultramarathon in Kitami City, Japan, on 24 June 2018.
                        """
                    ]
                ]
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
