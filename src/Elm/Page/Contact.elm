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
            [ text "If you have any wishes for improving the functionality or usability, you can easily contact us at this email address. Also, if you notice a bug please let us know with the attached screenshot or a detailed description of the problem. I am grateful to accept the proposals for adding new calculators." ]
        ]
