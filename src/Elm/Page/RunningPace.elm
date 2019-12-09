module Elm.Page.RunningPace exposing
    ( ExternalMsg(..)
    , Model
    , Msg(..)
    , init
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
import Task


type alias Model =
    { distanceInput : InputElement.Model
    , timeInput : InputElement.Model
    , result : ResultElement.Model
    , isCalculated : Bool
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = NoOp
    | DistanceInputMsg InputElement.Msg
    | TimeInputMsg InputElement.Msg
    | ResultMsg ResultElement.Msg
    | CalculatePace
    | ResetForm


type ExternalMsg
    = ShowSnackbar String
    | HideSnackbar


type Validation
    = ValidationError String
    | ValidationSuccess


type Calculation
    = CalculationError String
    | CalculationSuccess Int


init : Model
init =
    { distanceInput =
        InputElement.init <| UnitService.Distance UnitService.Kilometer
    , timeInput =
        InputElement.init UnitService.Time
    , result =
        ResultElement.init <| UnitService.Pace UnitService.PerKilometer
    , isCalculated = False
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        DistanceInputMsg (InputElement.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    InputElement.update subMsg model.distanceInput
            in
            ( { model
                | distanceInput = updatedModel
                , isCalculated =
                    if model.isCalculated then
                        model.distanceInput.value == updatedModel.value

                    else
                        False
              }
            , Cmd.batch
                [ Cmd.map (Self << DistanceInputMsg) cmd
                , if model.isCalculated && model.distanceInput.value /= updatedModel.value then
                    CmdUtil.fire <| (Self << ResultMsg) (ResultElement.Self ResultElement.ClearValue)

                  else
                    Cmd.none
                ]
            )

        DistanceInputMsg (InputElement.Parent subMsg) ->
            ( model
            , case subMsg of
                InputElement.ShowSnackbar message ->
                    CmdUtil.fire <| (Parent << ShowSnackbar) message
            )

        TimeInputMsg (InputElement.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    InputElement.update subMsg model.timeInput
            in
            ( { model
                | timeInput = updatedModel
                , isCalculated =
                    if model.isCalculated then
                        model.timeInput.value == updatedModel.value

                    else
                        False
              }
            , Cmd.batch
                [ Cmd.map (Self << TimeInputMsg) cmd
                , if model.isCalculated && model.timeInput.value /= updatedModel.value then
                    CmdUtil.fire <| (Self << ResultMsg) (ResultElement.Self ResultElement.ClearValue)

                  else
                    Cmd.none
                ]
            )

        TimeInputMsg (InputElement.Parent _) ->
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
                        CmdUtil.fire (Self CalculatePace)

                    else
                        Cmd.none
            )

        CalculatePace ->
            let
                error message =
                    ( model
                    , Cmd.batch
                        [ CmdUtil.fire ((Parent << ShowSnackbar) message)
                        , CmdUtil.fire <| (Self << ResultMsg) (ResultElement.Self ResultElement.ClearValue)
                        ]
                    )

                success int =
                    ( { model | isCalculated = True }
                    , Cmd.batch
                        [ CmdUtil.fire (Parent HideSnackbar)
                        , CmdUtil.fire
                            (int
                                |> ConverterService.secToPace
                                |> (ResultElement.Self << ResultElement.SetValue)
                                |> (Self << ResultMsg)
                            )
                        ]
                    )

                validation =
                    validate model.distanceInput model.timeInput
            in
            case validation of
                ValidationError message ->
                    error message

                ValidationSuccess ->
                    let
                        calculation =
                            calculate model.distanceInput model.timeInput model.result
                    in
                    case calculation of
                        CalculationSuccess sec ->
                            success sec

                        CalculationError message ->
                            error message

        ResetForm ->
            ( init
            , Cmd.none
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
                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t d

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.mToKm (truncate d))

                ( UnitService.Distance UnitService.Kilometer, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.kmToMi d)

                ( UnitService.Distance UnitService.Meter, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.mToMi (truncate d))

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t d

                ( UnitService.Distance UnitService.Mile, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.miToKm d)

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerMile ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.ydToMi (truncate d))

                ( UnitService.Distance UnitService.Yard, UnitService.Pace UnitService.PerKilometer ) ->
                    CalculationSuccess <| CalculatorService.pace t (CalculatorService.ydToKm (truncate d))

                _ ->
                    error

        _ ->
            error


view : Model -> Html Msg
view model =
    div [ class "running-pace-page" ]
        [ PageLayout.view
            { form =
                Html.form []
                    [ Html.map (Self << DistanceInputMsg) <|
                        InputElement.view
                            { name = "Running distance"
                            , units =
                                ( "pace-distance"
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
                            model.distanceInput
                    , Html.map (Self << TimeInputMsg) <|
                        InputElement.view
                            { name = "Running time"
                            , units =
                                ( ""
                                , [ { unit = UnitService.Time
                                    , name = ""
                                    , hint = "You should follow this pattern - HH:MM:SS"
                                    , shortcut = ""
                                    , regex = ValidatorService.timeRegex
                                    , error = "Wrong value, please make sure you added leading zeros and do not overreach time format (max: 23:59:59)"
                                    }
                                  ]
                                )
                            , links = []
                            }
                            model.timeInput
                    ]
            , result =
                div []
                    [ Html.map (Self << ResultMsg) <|
                        ResultElement.view
                            { title = "Your pace is"
                            , units =
                                ( "pace-result"
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
            , calculate =
                { msg = Self CalculatePace
                , isCalculated = model.isCalculated
                }
            , reset = Self ResetForm
            }
        ]
