module Elm.Layout.Page exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model msg =
    { form : Html msg
    , result : Html msg
    , calculate :
        { msg : msg
        , isCalculated : Bool
        }
    , reset : msg
    , link : String
    }


view : Model msg -> Html msg
view { form, result, calculate, reset, link } =
    div [ class "page-layout" ]
        [ div [ class "page-layout__side page-layout__side-left" ]
            [ form
            , div
                [ class "page-layout__buttons" ]
                [ node "custom-mwc-button"
                    [ class "page-layout__button"
                    , attribute "label" "Reset"
                    , onClick reset
                    ]
                    []
                , node "custom-mwc-button"
                    [ class "page-layout__button page-layout__button_calculate"
                    , classList [ ( "is-calculated", calculate.isCalculated ) ]
                    , attribute "raised" ""
                    , attribute "label"
                        (if calculate.isCalculated then
                            "Calculated"

                         else
                            "Calculate"
                        )
                    , onClick calculate.msg
                    ]
                    []
                ]
            , result
            ]
        , div [ class "page-layout__side page-layout__side-right" ]
            [ iframe
                [ class "page-layout__iframe"
                , src link
                ]
                []
            ]
        ]
