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
                                error <| ValidatorService.outOfRange "time"

                        CalculationError message ->
                            error message

        ResetForm ->
            ( { model
                | distance = InputElement.setValue "" model.distance
                , speed = InputElement.setValue "" model.speed
                , result = ResultElement.clearValue model.result
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

                { distance, speed } =
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
            ValidationError ValidatorService.fieldsNotValid

        ( False, True ) ->
            ValidationError <| ValidatorService.fieldNotValid "distance"

        ( True, False ) ->
            ValidationError <| ValidatorService.fieldNotValid "speed"

        ( True, True ) ->
            -- Fields are valid but can be empty, so check emptyness next
            case ( not <| String.isEmpty distance.value, not <| String.isEmpty speed.value ) of
                ( False, False ) ->
                    ValidationError <| ValidatorService.fieldsEmpty "time"

                ( False, True ) ->
                    ValidationError <| ValidatorService.fieldEmpty "distance"

                ( True, False ) ->
                    ValidationError <| ValidatorService.fieldEmpty "speed"

                ( True, True ) ->
                    -- Fields are valid and have value, so we can try to calculate pace
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> Calculation
calculate distance speed =
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
                    { name = "Cycling distance"
                    , units =
                        ( "cycling-time-distance"
                        , [ { unit = UnitService.Distance UnitService.Kilometer
                            , name = "Kilometers"
                            , hint = ValidatorService.floatHint
                            , shortcut = "km"
                            , regex = ValidatorService.floatRegex
                            , error = ValidatorService.floatError
                            }
                          , { unit = UnitService.Distance UnitService.Mile
                            , name = "Miles"
                            , hint = ValidatorService.floatHint
                            , shortcut = "mi"
                            , regex = ValidatorService.floatRegex
                            , error = ValidatorService.floatError
                            }
                          ]
                        )
                    , links =
                        [ { name = "Alpe d'Huez"
                          , value = "13.2"
                          , unit = UnitService.Distance UnitService.Kilometer
                          }
                        , { name = "Paris - Roubaix"
                          , value = "257"
                          , unit = UnitService.Distance UnitService.Kilometer
                          }
                        , { name = "Milan - San Remo"
                          , value = "185.2"
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
                [ text "Get your time" ]
            , p [ class "mdc-typography mdc-typography--body1" ]
                [ text
                    """
                    Knowing the time will help you understand
                    whether you can achieve the next goal, whether
                    it will be a group race or a cycling part of Ironman.
                    """
                ]
            , h1 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Try to use cycling time calculator to" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Determine the time of your next training session.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Determine the time you need to get to the target place.
                        """
                    ]
                ]
            , h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Interesting facts" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The Paris–Roubaix is a famous one-day professional men's bicycle road race in northern France.
                        The road of this race is very tough, covered with paving stones and earthen plots.
                        Punctures and falls occur very often. But still, the fastest time of this
                        race is 5 hours 41 minutes 7 seconds for a distance of 257 km (159.7 mi).
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The average time to complete Ironman 140.6 is 12 hours and 35 minutes.
                        180 km (112 mi) bike course takes approximately 6 hours 25 minutes.
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
