module Elm.Page.CyclingTime exposing
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
    , speed : InputElement.Model
    , result : ResultElement.Model
    , isCalculated : Bool
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = DistanceInputMsg InputElement.Msg
    | SpeedInputMsg InputElement.Msg
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
    , speed : String
    , speedUnit : Int
    , isCalculated : Bool
    }


db : String
db =
    "cycling-time"


storageEncoder : Storage -> Encode.Value
storageEncoder storage =
    Encode.object
        [ ( "distance", Encode.string storage.distance )
        , ( "distanceUnit", Encode.int storage.distanceUnit )
        , ( "speed", Encode.string storage.speed )
        , ( "speedUnit", Encode.int storage.speedUnit )
        , ( "isCalculated", Encode.bool storage.isCalculated )
        ]


storageDecoder : Decode.Decoder Storage
storageDecoder =
    Decode.map5 Storage
        (Decode.at [ "distance" ] Decode.string)
        (Decode.at [ "distanceUnit" ] Decode.int)
        (Decode.at [ "speed" ] Decode.string)
        (Decode.at [ "speedUnit" ] Decode.int)
        (Decode.at [ "isCalculated" ] Decode.bool)


init : ( Model, Cmd Msg )
init =
    ( { distance =
            InputElement.init <| UnitService.Distance UnitService.Kilometer
      , speed =
            InputElement.init <| UnitService.Speed UnitService.KilometersPerHour
      , result =
            ResultElement.init UnitService.Time
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

                InputElement.UnitChanged ->
                    ( model
                    , if model.isCalculated then
                        CmdUtil.fire <| Self CalculateTime

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
                                , speed = model.speed.value
                                , speedUnit = UnitService.toId model.speed.unit
                                , isCalculated = True
                                }
                            )
                        ]
                    )

                validation =
                    validate model.distance model.speed
            in
            case validation of
                ValidationError message ->
                    error message

                ValidationSuccess ->
                    let
                        calculation =
                            calculate model.distance model.speed
                    in
                    case calculation of
                        CalculationSuccess sec ->
                            if BasicsExtra.isSafeInteger sec then
                                success sec

                            else
                                error "The time is out of range, please insert smaller values of distance or speed"

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | distance = InputElement.setValue "" model.distance
                , speed = InputElement.setValue "" model.speed
                , result = ResultElement.setValue "..." model.result
                , isCalculated = False
              }
            , Port.saveToLocalStorage
                ( db
                , storageEncoder
                    { distance = ""
                    , distanceUnit = UnitService.toId model.distance.unit
                    , speed = ""
                    , speedUnit = UnitService.toId model.speed.unit
                    , isCalculated = False
                    }
                )
            )

        LocalStorageResponse value ->
            let
                decodeStorage =
                    Decode.decodeValue storageDecoder value

                { distance, speed, result } =
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

                        updatedSpeedInput =
                            { speed
                                | value = storage.speed
                                , unit = UnitService.fromId storage.speedUnit
                            }
                    in
                    ( { model
                        | distance = updatedDistanceInput
                        , speed = updatedSpeedInput
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
validate distance speed =
    case ( distance.isValid, speed.isValid ) of
        ( False, False ) ->
            ValidationError "Both fields are not valid, please take a look at the hints"

        ( True, False ) ->
            ValidationError "Not valid value for speed"

        ( False, True ) ->
            ValidationError "Not valid value for distance"

        ( True, True ) ->
            -- Fields are valid but can be empty, so check emptyness next
            case ( not <| String.isEmpty distance.value, not <| String.isEmpty speed.value ) of
                ( False, False ) ->
                    ValidationError "Both fields are empty. Please fill the fields to calculate speed"

                ( True, False ) ->
                    ValidationError "Please add speed value"

                ( False, True ) ->
                    ValidationError "Please add distance value"

                ( True, True ) ->
                    -- Fields are valid and have value, so we can try to calculate pace
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> Calculation
calculate distance speed =
    let
        error =
            CalculationError "Something went wrong. Please contact us"
    in
    case ( String.toFloat distance.value, String.toFloat speed.value ) of
        ( Just d, Just s ) ->
            case ( distance.unit, speed.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Speed UnitService.KilometersPerHour ) ->
                    CalculationSuccess <| round (3600 * CalculatorService.time2 d s)

                ( UnitService.Distance UnitService.Mile, UnitService.Speed UnitService.MilesPerHour ) ->
                    CalculationSuccess <| round (3600 * CalculatorService.time2 d s)

                ( UnitService.Distance UnitService.Kilometer, UnitService.Speed UnitService.MilesPerHour ) ->
                    CalculationSuccess <| round (3600 * CalculatorService.time2 d (CalculatorService.miPerHToKmPerH s))

                ( UnitService.Distance UnitService.Mile, UnitService.Speed UnitService.KilometersPerHour ) ->
                    CalculationSuccess <| round (3600 * CalculatorService.time2 d (CalculatorService.kmPerHToMiPerH s))

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
                        { name = "Cycling distance"
                        , units =
                            ( "cycling-time-distance"
                            , [ { unit = UnitService.Distance UnitService.Kilometer
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
                            [ { name = "Alpe d'Huez"
                              , value = "13.2"
                              , unit = UnitService.Distance UnitService.Kilometer
                              }
                            , { name = "Paris - Roubaix"
                              , value = "150"
                              , unit = UnitService.Distance UnitService.Mile
                              }
                            , { name = "Milan - San Remo"
                              , value = "185"
                              , unit = UnitService.Distance UnitService.Mile
                              }
                            ]
                        }
                        model.distance
                , Html.map (Self << SpeedInputMsg) <|
                    InputElement.view
                        { name = "Cycling speed"
                        , units =
                            ( "cycling-time-speed"
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
            div []
                [ Html.map (Self << ResultMsg) <|
                    ResultElement.view
                        { title = "Your time is"
                        , units =
                            ( ""
                            , []
                            )
                        }
                        model.result
                ]

        description =
            div []
                [ h2 [ class "mdc-typography mdc-typography--headline4" ] [ text "Who Uses a Cycling Time Calculator?" ]
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
