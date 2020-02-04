module Elm.Page.Home exposing
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


type alias Button =
    { label : String
    , link : String
    }


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view _ =
    div [ class "home-page" ]
        [ div [ class "home-page__cards" ]
            [ viewCard
                "Running"
                "What pace should be to run a marathon less than 4 hours? How much time take to run 100 meters? Calculate it here"
                "images/icons/running.svg"
                [ { label = "Pace"
                  , link = "/running/pace"
                  }
                , { label = "Time"
                  , link = "/running/time"
                  }
                , { label = "Distance"
                  , link = "/running/distance"
                  }
                ]
            , viewCard
                "Cycling"
                "Does speed matter for you? Find out speed, time or distance for your next ride"
                "images/icons/cycling.svg"
                [ { label = "Speed"
                  , link = "/cycling/speed"
                  }
                , { label = "Time"
                  , link = "/cycling/time"
                  }
                , { label = "Distance"
                  , link = "/cycling/distance"
                  }
                ]
            , viewCard
                "Swimming"
                "What pace to choose for your next swim session? Wanna know Michael Phelps pace? Get it here"
                "images/icons/swimming.svg"
                [ { label = "Pace"
                  , link = "/swimming/pace"
                  }
                , { label = "Time"
                  , link = "/swimming/time"
                  }
                , { label = "Distance"
                  , link = "/swimming/distance"
                  }
                ]
            ]
        , div [ class "home-page__description" ]
            [ h1 [ class "mdc-typography mdc-typography--body1" ]
                [ text
                    """
                    Speed Matters app combines calculators for running, cycling and swimming.
                    With their help, you can easily calculate the various metrics in these sports.
                    Preparing for the first marathon distance, the next open water race or a long bike ride,
                    you will most likely need to calculate the pace, speed, distance or time.
                    For both the amateurs and the professional athletes,
                    one of the most common routines is to calculate their sport metrics.
                    If you want to plan your result in upcoming competitions or find out the right pace for a personal record,
                    our app will help you with this.
                    It will be convenient for building training plans in the triathlon, as well as for each discipline separately.
                    """
                ]
            ]
        ]


viewCard : String -> String -> String -> List Button -> Html Msg
viewCard title content image buttons =
    let
        button : Button -> Html Msg
        button { label, link } =
            a
                [ class "home-page__card-link"
                , href link
                ]
                [ node "custom-mwc-button"
                    [ attribute "label" label
                    ]
                    []
                ]
    in
    div [ class "mdc-card home-page__card" ]
        [ div
            [ class "mdc-card__media mdc-card__media--square home-page__card-media"
            , style "background-image" ("url(" ++ image ++ ")")
            ]
            []
        , div [ class "home-page__card-content" ]
            [ h2 [ class "mdc-typography mdc-typography--headline6 home-page__card-title" ]
                [ text title ]
            , p [ class "mdc-typography mdc-typography--body2 home-page__card-text" ]
                [ text content ]
            ]
        , div [ class "mdc-card__actions home-page__actions" ]
            [ div [ class "mdc-card__action-buttons home-page__card-buttons" ]
                (List.map button buttons)
            ]
        ]
