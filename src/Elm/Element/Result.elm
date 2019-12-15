module Elm.Element.Result exposing
    ( ExternalMsg(..)
    , Model
    , Msg(..)
    , init
    , update
    , view
    )

import Elm.Service.Unit as UnitService
import Elm.Util.Cmd as CmdUtil
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { value : String
    , unit : UnitService.Unit
    }


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = ChangeUnit UnitService.Unit


type ExternalMsg
    = UnitChanged


type alias Settings =
    { title : String
    , units : ( String, List UnitData )
    }


type alias UnitData =
    { unit : UnitService.Unit
    , name : String
    , shortcut : String
    }


init : UnitService.Unit -> Model
init unit =
    { value = "..."
    , unit = unit
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUnit unit ->
            ( { model | unit = unit }
            , CmdUtil.fire (Parent UnitChanged)
            )


view : Settings -> Model -> Html Msg
view { title, units } model =
    let
        activeUnitData =
            List.filter (\ud -> ud.unit == model.unit) (Tuple.second units)
    in
    div [ class "result-element" ]
        [ div [ class "result-element__main" ]
            [ span [ class "result-element__title mdc-typography--headline4" ] [ text title ]
            , span
                [ class "result-element__value mdc-typography--headline3"
                , attribute "title" model.value
                ]
                [ text model.value
                ]
            , span [ class "result-element__shortcut mdc-typography--headline5" ]
                [ List.head activeUnitData
                    |> Maybe.map (\ud -> "(" ++ ud.shortcut ++ ")")
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
        , div [ class "result-element__scroller" ]
            [ case Tuple.second units of
                [] ->
                    text ""

                xs ->
                    div [ class "result-element__scroller" ]
                        (List.map
                            (\u ->
                                viewUnit
                                    u
                                    (Tuple.first units)
                                    (u.unit == model.unit)
                            )
                            (Tuple.second units)
                        )
            ]
        ]


viewUnit : UnitData -> String -> Bool -> Html Msg
viewUnit { unit, name } group isChecked =
    div [ class "result-element__item" ]
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
