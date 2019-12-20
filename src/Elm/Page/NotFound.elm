module Elm.Page.NotFound exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ class "not-found-page" ]
        [ i [ class "not-found-page__icon material-icons" ]
            [ text "image_search" ]
        , h2 [ class "mdc-typography mdc-typography--headline4" ] [ text "Page not found" ]
        , a
            [ class "not-found-page__link"
            , href "/"
            ]
            [ node "custom-mwc-button"
                [ attribute "label" "Return to home page"
                , attribute "raised" ""
                ]
                []
            ]
        ]
