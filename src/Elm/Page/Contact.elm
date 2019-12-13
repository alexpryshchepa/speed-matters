module Elm.Page.Contact exposing
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
    div [ class "contact-page" ]
        [ h2 [ class "mdc-typography mdc-typography--headline4" ] [ text "Contact us" ]
        , p [ class "mdc-typography mdc-typography--body1" ]
            [ text "If you have any suggestion or advice you can simply contact us by one of the following ways" ]
        , ul []
            [ li [ class "mdc-typography mdc-typography--body1" ]
                [ text "email" ]
            , li [ class "mdc-typography mdc-typography--body1" ]
                [ text "telegram" ]
            , li [ class "mdc-typography mdc-typography--body1" ]
                [ text "whatsapp" ]
            ]
        ]
