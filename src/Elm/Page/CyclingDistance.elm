module Elm.Page.CyclingDistance exposing
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
    , speed : InputElement.Model
    , result : ResultElement.Model
    , isCalculated : Bool
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = TimeInputMsg InputElement.Msg
    | SpeedInputMsg InputElement.Msg
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
    , speed : String
    , speedUnit : Int
    , distanceUnit : Int
    , isCalculated : Bool
    }


db : String
db =
    "cycling-distance"


storageEncoder : Storage -> Encode.Value
storageEncoder storage =
    Encode.object
        [ ( "time", Encode.string storage.time )
        , ( "speed", Encode.string storage.speed )
        , ( "speedUnit", Encode.int storage.speedUnit )
        , ( "distanceUnit", Encode.int storage.distanceUnit )
        , ( "isCalculated", Encode.bool storage.isCalculated )
        ]


storageDecoder : Decode.Decoder Storage
storageDecoder =
    Decode.map5 Storage
        (Decode.at [ "time" ] Decode.string)
        (Decode.at [ "speed" ] Decode.string)
        (Decode.at [ "speedUnit" ] Decode.int)
        (Decode.at [ "distanceUnit" ] Decode.int)
        (Decode.at [ "isCalculated" ] Decode.bool)


init : ( Model, Cmd Msg )
init =
    ( { time =
            InputElement.init UnitService.Time
      , speed =
            InputElement.init <| UnitService.Speed UnitService.KilometersPerHour
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

        SpeedInputMsg (InputElement.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    InputElement.update subMsg model.speed
            in
            ( { model | speed = updatedModel }
            , Cmd.map (Self << SpeedInputMsg) cmd
            )

        SpeedInputMsg (InputElement.Parent subMsg) ->
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
                                , speed = model.speed.value
                                , speedUnit = UnitService.toId model.speed.unit
                                , distanceUnit = UnitService.toId model.result.unit
                                , isCalculated = True
                                }
                            )
                        ]
                    )

                validation =
                    validate model.time model.speed
            in
            case validation of
                ValidationError message ->
                    error message

                ValidationSuccess ->
                    let
                        calculation =
                            calculate model.time model.speed model.result
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
                , speed = InputElement.setValue "" model.speed
                , result = ResultElement.clearValue model.result
                , isCalculated = False
              }
            , Port.saveToLocalStorage
                ( db
                , storageEncoder
                    { time = ""
                    , speed = ""
                    , speedUnit = UnitService.toId model.speed.unit
                    , distanceUnit = UnitService.toId model.result.unit
                    , isCalculated = False
                    }
                )
            )

        LocalStorageResponse value ->
            let
                decodeStorage =
                    Decode.decodeValue storageDecoder value

                { time, speed, result } =
                    model
            in
            case decodeStorage of
                Ok storage ->
                    let
                        updatedTimeInput =
                            { time
                                | value = storage.time
                            }

                        updatedSpeedInput =
                            { speed
                                | value = storage.speed
                                , unit = UnitService.fromId storage.speedUnit
                            }

                        updatedResultInput =
                            { result | unit = UnitService.fromId storage.distanceUnit }
                    in
                    ( { model
                        | time = updatedTimeInput
                        , speed = updatedSpeedInput
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
validate time speed =
    case ( time.isValid, speed.isValid ) of
        ( False, False ) ->
            ValidationError ValidatorService.fieldsNotValid

        ( False, True ) ->
            ValidationError <| ValidatorService.fieldNotValid "time"

        ( True, False ) ->
            ValidationError <| ValidatorService.fieldNotValid "speed"

        ( True, True ) ->
            case ( not <| String.isEmpty time.value, not <| String.isEmpty speed.value ) of
                ( False, False ) ->
                    ValidationError <| ValidatorService.fieldsEmpty "distance"

                ( False, True ) ->
                    ValidationError <| ValidatorService.fieldEmpty "time"

                ( True, False ) ->
                    ValidationError <| ValidatorService.fieldEmpty "speed"

                ( True, True ) ->
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> ResultElement.Model -> Calculation
calculate time speed result =
    case ( ConverterService.timeToHour time.value, String.toFloat speed.value ) of
        ( Just t, Just s ) ->
            case ( result.unit, speed.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Speed UnitService.KilometersPerHour ) ->
                    CalculationSuccess <| CalculatorService.distance2 t s

                ( UnitService.Distance UnitService.Mile, UnitService.Speed UnitService.MilesPerHour ) ->
                    CalculationSuccess <| CalculatorService.distance2 t s

                ( UnitService.Distance UnitService.Kilometer, UnitService.Speed UnitService.MilesPerHour ) ->
                    CalculationSuccess <| CalculatorService.distance2 t (CalculatorService.miPerHToKmPerH s)

                ( UnitService.Distance UnitService.Mile, UnitService.Speed UnitService.KilometersPerHour ) ->
                    CalculationSuccess <| CalculatorService.distance2 t (CalculatorService.kmPerHToMiPerH s)

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
                    { name = "Cycling time"
                    , units =
                        ( "cycling-distance-time"
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
            , Html.map (Self << SpeedInputMsg) <|
                InputElement.view
                    { name = "Cycling speed"
                    , units =
                        ( "cycling-distance-speed"
                        , [ { unit = UnitService.Speed UnitService.KilometersPerHour
                            , name = "Kilometers"
                            , hint = "Must be a number, e.g 33.5"
                            , shortcut = "km/h"
                            , regex = ValidatorService.floatRegex
                            , error = "Wrong value, please make sure you value is float or integer number e.g. 18"
                            }
                          , { unit = UnitService.Speed UnitService.MilesPerHour
                            , name = "Miles"
                            , hint = "Must be a number, e.g 33.5"
                            , shortcut = "mi/h"
                            , regex = ValidatorService.floatRegex
                            , error = "Wrong value, please make sure you value is float or integer number e.g. 18"
                            }
                          ]
                        )
                    , links =
                        []
                    }
                    model.speed
            ]

        result =
            [ Html.map (Self << ResultMsg) <|
                ResultElement.view
                    { title = "Distance is"
                    , units =
                        ( "cycling-distance-result"
                        , [ { name = "Kilometers"
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
            [ h2 [ class "mdc-typography mdc-typography--headline4" ] [ text "CONTENT" ]
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
