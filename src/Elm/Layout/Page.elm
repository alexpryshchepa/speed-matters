module Elm.Layout.Page exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model msg =
    { form : Html msg
    , result : Html msg
    , button :
        { msg : msg
        , isCalculated : Bool
        }
    }


view : Model msg -> Html msg
view { form, result, button } =
    div [ class "page-layout" ]
        [ div [ class "page-layout__side page-layout__side-left" ]
            [ form
            , div
                [ class "page-layout__calculate"
                , classList [ ( "is-calculated", button.isCalculated ) ]
                ]
                [ node "custom-mwc-button"
                    [ attribute "raised" ""
                    , attribute "label"
                        (if button.isCalculated then
                            "Calculated"

                         else
                            "Calculate"
                        )
                    , onClick button.msg
                    ]
                    []
                ]
            , result
            ]
        , div [ class "page-layout__side page-layout__side-right" ]
            []
        ]
