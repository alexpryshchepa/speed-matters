module Elm.Element.Input exposing
    ( ExternalMsg(..)
    , Model
    , Msg(..)
    , init
    , setValue
    , update
    , view
    )

import Elm.Service.Unit as UnitService
import Elm.Service.Validator as ValidatorService
import Elm.Util.Cmd as CmdUtil
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Task


type alias Model =
    { value : String
    , unit : UnitService.Unit
    , isValid : Bool
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = ChangeValue String
    | ChangeUnit UnitService.Unit
    | UpdateValue UnitService.Unit String
    | ChangeValidity Bool


type ExternalMsg
    = ShowSnackbar String
    | ValueChanged String String
    | ConvertationFailed
    | UnitChanged


type alias Settings =
    { name : String
    , units : ( String, List UnitData )
    , links : List Link
    }


type alias UnitData =
    { unit : UnitService.Unit
    , name : String
    , hint : String
    , shortcut : String
    , regex : String
    , error : String
    }


type alias Link =
    { name : String
    , unit : UnitService.Unit
    , value : String
    }


init : UnitService.Unit -> Model
init unit =
    { value = ""
    , unit = unit
    , isValid = True
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeValue value ->
            let
                trimmed =
                    String.trim value
            in
            ( { model | value = trimmed }
            , CmdUtil.fire <| Parent (ValueChanged model.value trimmed)
            )

        ChangeUnit unit ->
            let
                convert =
                    UnitService.convert model.unit unit model.value
            in
            ( { model
                | unit = unit
              }
            , Cmd.batch
                [ CmdUtil.fire <| Parent UnitChanged
                , case convert of
                    UnitService.Converted v ->
                        CmdUtil.fire <| (Self << ChangeValue) v

                    UnitService.ConvertationSkipped ->
                        Cmd.none

                    UnitService.ConvertationFailed ->
                        if String.isEmpty model.value then
                            Cmd.none

                        else
                            Cmd.batch
                                [ CmdUtil.fire <| (Parent << ShowSnackbar) "Cannot convert this value"
                                , CmdUtil.fire <| Parent ConvertationFailed
                                ]
                ]
            )

        UpdateValue unit value ->
            let
                convert =
                    UnitService.convert unit model.unit value
            in
            ( model
            , case convert of
                UnitService.Converted v ->
                    CmdUtil.fire <| (Self << ChangeValue) v

                UnitService.ConvertationSkipped ->
                    CmdUtil.fire <| (Self << ChangeValue) value

                UnitService.ConvertationFailed ->
                    CmdUtil.fire <| (Parent << ShowSnackbar) "Something went wrong. Please contact us"
            )

        ChangeValidity bool ->
            ( { model | isValid = bool }
            , Cmd.none
            )



-- Lenses


setValue : String -> Model -> Model
setValue value model =
    { model | value = value }


view : Settings -> Model -> Html Msg
view { name, units, links } model =
    let
        activeUnitData fn attr default =
            List.filter (\ud -> ud.unit == model.unit) (Tuple.second units)
                |> List.head
                |> Maybe.map fn
                |> Maybe.withDefault default
                |> attribute attr
    in
    div [ class "input-element" ]
        [ div [ class "input-element__field" ]
            [ node "custom-mwc-textfield"
                [ attribute "outlined" ""
                , attribute "label" name
                , attribute "icontrailing" "delete"
                , attribute "value" model.value
                , activeUnitData .error "validationmessage" "Not valid"
                , activeUnitData .regex "regex" ""
                , activeUnitData .hint "helper" ""
                , activeUnitData .shortcut "unit" ""
                , onInput (Self << ChangeValue)
                , on "MDCTextfield:delete" (Decode.succeed <| (Self << ChangeValue) "")
                , on "MDCTextfield:validated" (Decode.map (Self << ChangeValidity) (Decode.field "detail" Decode.bool))
                ]
                []
            ]
        , case Tuple.second units of
            [] ->
                text ""

            [ x ] ->
                text ""

            xs ->
                div [ class "input-element__scroller" ]
                    (List.map
                        (\u ->
                            viewUnit
                                u
                                (Tuple.first units)
                                (u.unit == model.unit)
                        )
                        (Tuple.second units)
                    )
        , case links of
            [] ->
                text ""

            xs ->
                div [ class "input-element__scroller" ]
                    (List.map viewLink links)
        ]


viewUnit : UnitData -> String -> Bool -> Html Msg
viewUnit { unit, name } group isChecked =
    div [ class "input-element__item" ]
        [ node "mwc-formfield"
            [ attribute "label" name ]
            [ node "mwc-radio"
                [ attribute "name" group
                , checked isChecked
                , onCheck (\_ -> (Self << ChangeUnit) unit)
                ]
                []
            ]
        ]


viewLink : Link -> Html Msg
viewLink { name, unit, value } =
    div [ class "input-element__item" ]
        [ node "custom-mwc-button"
            [ attribute "outlined" ""
            , attribute "dense" ""
            , attribute "label" name
            , onClick <| Self (UpdateValue unit value)
            ]
            []
        ]
