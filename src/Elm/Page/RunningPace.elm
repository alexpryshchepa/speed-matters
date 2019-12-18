module Elm.Page.RunningPace exposing
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
    "running-pace"


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
            InputElement.init <| UnitService.Distance UnitService.Kilometer
      , time =
            InputElement.init UnitService.Time
      , result =
            ResultElement.init <| UnitService.Pace UnitService.PerKilometer
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
                        CmdUtil.fire <| Self CalculatePace

                      else
                        Cmd.none
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
                        CmdUtil.fire <| Self CalculatePace

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
                        CmdUtil.fire <| Self CalculatePace

                    else
                        Cmd.none
            )

        CalculatePace ->
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
                                error <| ValidatorService.outOfRange "pace"

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | distance = InputElement.setValue "" model.distance
                , time = InputElement.setValue "" model.time
                , result = ResultElement.clearValue model.result
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
            ValidationError ValidatorService.fieldsNotValid

        ( False, True ) ->
            ValidationError <| ValidatorService.fieldNotValid "distance"

        ( True, False ) ->
            ValidationError <| ValidatorService.fieldNotValid "time"

        ( True, True ) ->
            case ( not <| String.isEmpty distance.value, not <| String.isEmpty time.value ) of
                ( False, False ) ->
                    ValidationError <| ValidatorService.fieldsEmpty "pace"

                ( False, True ) ->
                    ValidationError <| ValidatorService.fieldEmpty "distance"

                ( True, False ) ->
                    ValidationError <| ValidatorService.fieldEmpty "time"

                ( True, True ) ->
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> ResultElement.Model -> Calculation
calculate distance time result =
    case ( String.toFloat distance.value, ConverterService.timeToSec time.value ) of
        ( Just d, Just t ) ->
            case ( distance.unit, result.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t d

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.mToKm (round d))

                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.kmToMi d)

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.mToMi (round d))

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t d

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.miToKm d)

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.ydToMi (round d))

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.ydToKm (round d))

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
            [ Html.map (Self << DistanceInputMsg) <|
                InputElement.view
                    { name = "Running distance"
                    , units =
                        ( "running-pace-distance"
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
            , Html.map (Self << TimeInputMsg) <|
                InputElement.view
                    { name = "Running time"
                    , units =
                        ( "running-pace-time"
                        , [ { unit = UnitService.Time
                            , name = ""
                            , hint = ValidatorService.timeHint
                            , shortcut = ""
                            , regex = ValidatorService.timeRegex
                            , error = ValidatorService.timeError
                            }
                          ]
                        )
                    , links = []
                    }
                    model.time
            ]

        result =
            [ Html.map (Self << ResultMsg) <|
                ResultElement.view
                    { title = "Pace is"
                    , units =
                        ( "running-pace-result"
                        , [ { name = "Per kilometer"
                            , unit = UnitService.Pace UnitService.PerKilometer
                            , shortcut = "min/km"
                            }
                          , { name = "Per mile"
                            , unit = UnitService.Pace UnitService.PerMile
                            , shortcut = "min/mi"
                            }
                          ]
                        )
                    }
                    model.result
            ]

        description =
            [ h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "What is running pace?" ]
            , p [ class "mdc-typography mdc-typography--body1" ]
                [ text
                    """
                    Pace is the term that runners use for the way they measure their speed while running.
                    While speed is generally measured as miles per hour or kilometers per hour,
                    runners do not have the patience to wait a whole hour to know how fast they have been running.
                    Instead, runners measure their speed in terms of minutes per mile or minutes per kilometer.
                    This type of speed measurement is what runners call pace.
                    """
                ]
            , h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Some examples" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Determine how fast your pace should be if you have a certain finish time for the next distance or race.
                        For example, find out what pace you need to keep to run
                        10 miles less than 40 minutes or a sub 2:00 half marathon.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Determine what pace needed to run to break the world record for the marathon.
                        The current world record time for men over the distance is 2 hours 1 minute and 39 seconds.
                        """
                    ]
                ]
            , h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Interesting facts" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The current world record time for men over the distance is 2 hours 1 minute and 39 seconds,
                        set in the Berlin Marathon by Eliud Kipchoge of Kenya on 16 September 2018,
                        an improvement of 1 minute 18 seconds over the previous record also set in the Berlin Marathon
                        by Dennis Kipruto Kimetto, also of Kenya on 28 September 2014.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The world record for women was set by Paula Radcliffe of Great Britain
                        in the London Marathon on 13 April 2003, in 2 hours 15 minutes and 25 seconds.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Fauja Singh, finished the Toronto Waterfront Marathon,
                        becoming the first centenarian ever to officially complete that distance. Singh,
                        a British citizen, finished the race on 16 October 2011 with a time of 8:11:05.9,
                        making him the oldest marathoner. Because Singh could not produce a birth certificate
                        from rural 1911 Colonial India, the place of his birth, his age could not be verified
                        and his record was not accepted by the official governing body World Masters Athletics.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The youngest under 4 hours is Mary Etta Boitano at age 7 years, 284 days;
                        under 3 hours Julie Mullin at 10 years 180 days;
                        and under 2:50 Carrie Garritson at 11 years 116 days.
                        """
                    ]
                ]
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
