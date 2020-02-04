module Elm.Page.CyclingSpeed exposing
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
    | CalculateSpeed
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
    { distance : String
    , distanceUnit : Int
    , time : String
    , resultUnit : Int
    , isCalculated : Bool
    }


db : String
db =
    "cycling-speed"


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
            ResultElement.init <| UnitService.Speed UnitService.KilometersPerHour
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
                        CmdUtil.fire <| Self CalculateSpeed

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
                        CmdUtil.fire <| Self CalculateSpeed

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
                        CmdUtil.fire <| Self CalculateSpeed

                    else
                        Cmd.none
            )

        CalculateSpeed ->
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
                        CalculationSuccess speed ->
                            if BasicsExtra.isSafeInteger <| floor speed then
                                success speed

                            else
                                error <| ValidatorService.outOfRange "speed"

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
                        CmdUtil.fire <| Self CalculateSpeed

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
                    ValidationError <| ValidatorService.fieldsEmpty "speed"

                ( False, True ) ->
                    ValidationError <| ValidatorService.fieldEmpty "distance"

                ( True, False ) ->
                    ValidationError <| ValidatorService.fieldEmpty "time"

                ( True, True ) ->
                    ValidationSuccess


calculate : InputElement.Model -> InputElement.Model -> ResultElement.Model -> Calculation
calculate distance time result =
    case ( String.toFloat distance.value, ConverterService.timeToHour time.value ) of
        ( Just d, Just t ) ->
            case ( distance.unit, result.unit ) of
                ( UnitService.Distance UnitService.Kilometer, UnitService.Speed UnitService.KilometersPerHour ) ->
                    CalculationSuccess <| CalculatorService.speed d t

                ( UnitService.Distance UnitService.Mile, UnitService.Speed UnitService.MilesPerHour ) ->
                    CalculationSuccess <| CalculatorService.speed d t

                ( UnitService.Distance UnitService.Kilometer, UnitService.Speed UnitService.MilesPerHour ) ->
                    CalculationSuccess <| CalculatorService.speed (CalculatorService.kmToMi d) t

                ( UnitService.Distance UnitService.Mile, UnitService.Speed UnitService.KilometersPerHour ) ->
                    CalculationSuccess <| CalculatorService.speed (CalculatorService.miToKm d) t

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
                        ( "cycling-speed-distance"
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
            , Html.map (Self << TimeInputMsg) <|
                InputElement.view
                    { name = "Cycling time"
                    , units =
                        ( "cycling-speed-time"
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
                    { title = "Speed is"
                    , units =
                        ( "cycling-speed-result"
                        , [ { name = "Kilometers"
                            , unit = UnitService.Speed UnitService.KilometersPerHour
                            , shortcut = "km/h"
                            }
                          , { name = "Miles"
                            , unit = UnitService.Speed UnitService.MilesPerHour
                            , shortcut = "mi/h"
                            }
                          ]
                        )
                    }
                    model.result
            ]

        description =
            [ h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Get your speed" ]
            , p [ class "mdc-typography mdc-typography--body1" ]
                [ text
                    """
                    Of course, you can calculate not only the speed of cycling, but the bike is our focus.
                    The average speed of cycling can reach 40 km per hour on a flat surface.
                    On cycling competitions, the speed can be even greater since the aerodynamic
                    effect of the peloton works there. Trying to reach your maximum speed do not forget about the traffic rules 😉
                    """
                ]
            , h1 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Try to use cycling speed calculator to" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Determine how fast you are on the last ride.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        Find out the speed you should ride to get to work on time.
                        """
                    ]
                ]
            , h2 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Interesting facts" ]
            , ul []
                [ li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The fastest human to ride a bicycle over open ground is named Denise Mueller-Korenek,
                        who rode a custom bike at an average of 184 miles per hour –
                        shattering a world record that had stood since 1995.
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The fastest Tour de France was ridden by Lance Armstrong 3592.5 km
                        in 86 hours 15 minutes 02 seconds – at an average speed of 41.7 km/h (25.9 mi/h).
                        """
                    ]
                , li [ class "mdc-typography mdc-typography--body1" ]
                    [ text
                        """
                        The bike course on Ironman competition is 180 km long.
                        The fastest athletes can ride it less than 4 hours with an average speed of 45 km/h.
                        """
                    ]
                ]
            ]
    in
    PageLayout.view
        { form = form
        , result = result
        , calculate =
            { msg = Self CalculateSpeed
            , isCalculated = model.isCalculated
            }
        , reset = Self ResetForm
        , description = description
        }
