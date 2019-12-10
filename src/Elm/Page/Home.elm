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
view model =
    div [ class "home-page" ]
        [ div [ class "home-page__heading" ]
            [ h1 [ class "mdc-typography mdc-typography--headline4" ]
                [ text "Most useful sport calculators in one place" ]
            , p [ class "mdc-typography mdc-typography--body1" ]
                [ text "Maroon matey hogshead cackle fruit sloop gaff hulk hardtack ahoy belay. Deadlights jack topgallant reef aft mutiny hogshead tackle piracy interloper. Lanyard dance the hempen jig yard boatswain belaying pin Brethren of the Coast reef sails American Main maroon jolly boat.\n\nJolly boat draught rutters fore dance the hempen jig fire in the hole starboard bring a spring upon her cable lookout coxswain. Fluke chantey no prey, no pay yardarm cutlass plunder boatswain tack grog blossom run a shot across the bow. Schooner nipperkin Blimey hands square-rigged brig belay carouser hail-shot interloper.\n\nLong boat bowsprit spanker American Main scallywag handsomely grog blossom wherry lugsail main sheet. To go on account cackle fruit tack gabion deadlights nipperkin log Shiver me timbers provost squiffy. Arr belaying pin yawl scurvy Pieces of Eight swab parrel pillage chase Shiver me timbers." ]
            ]
        , div [ class "home-page__cards" ]
            [ viewCard
                "Running"
                "Most important calculators"
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
                "Get some useful data"
                "images/icons/cycling.svg"
                [ { label = "Estimated power"
                  , link = "/cycling/power"
                  }
                , { label = "Time"
                  , link = "/cycling/time"
                  }
                , { label = "Distance"
                  , link = "/cycling/distance"
                  }
                , { label = "Speed"
                  , link = "/cycling/speed"
                  }
                ]
            , viewCard
                "Swimming"
                "Predict you results with awesome swimming calculators"
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
        , div [ class "mdc-card__actions" ]
            [ div [ class "mdc-card__action-buttons home-page__card-buttons" ]
                (List.map button buttons)
            ]
        ]
