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


type alias Model = String


type Msg
    = ShowEmail


init : Model
init = ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ShowEmail ->
            ( "spdmttrs@gmail.com"
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "contact-page" ]
        [ div [ class "contact-page__content" ]
            [ h1 [ class "mdc-typography mdc-typography--headline4" ] [ text "Contact us" ]
            , p [ class "mdc-typography mdc-typography--body1" ]
                [ text
                    """
                    If you have any advice for improving the functionality or usability,
                    please drop us a line by
                    """
                , span
                    [ class "contact-page__link"
                    , onClick ShowEmail
                    ]
                    [ text "email" ]
                , text "."
                , text
                    """
                    We will be very glad to hear your suggestions for adding new calculators.
                    Also, if you notice a bug please let us know with the attached screenshots
                    or a detailed description of the problem.
                    Thanks! May the speed be with you! 🏃
                    """
                ]
            , div
                [ class "contact-page__contact"
                , classList [ ( "is-visible", not <| String.isEmpty model ) ]
                ]
                [ node "custom-mwc-button"
                    [ class "contact-page__button"
                    , attribute "outlined" ""
                    , attribute "dense" ""
                    , attribute "label" "Show email"
                    , onClick ShowEmail
                    ]
                    []
                , a
                    [ class "contact-page__email mdc-typography mdc-typography--body1"
                    , href ("mailto:" ++ model)
                    ]
                    [ text model ]
                ]
            ]
        , div [ class "contact-page__image" ]
            []
        ]
