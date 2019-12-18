module Elm.Route exposing
    ( ExternalMsg(..)
    , Model
    , Msg(..)
    , Route(..)
    , init
    , subscriptions
    , update
    , view
    )

import Elm.Page.Contact as ContactPage
import Elm.Page.CyclingDistance as CyclingDistancePage
import Elm.Page.CyclingSpeed as CyclingSpeedPage
import Elm.Page.CyclingTime as CyclingTimePage
import Elm.Page.Home as HomePage
import Elm.Page.NotFound as NotFoundPage
import Elm.Page.RunningDistance as RunningDistancePage
import Elm.Page.RunningPace as RunningPacePage
import Elm.Page.RunningTime as RunningTimePage
import Elm.Page.SwimmingDistance as SwimmingDistancePage
import Elm.Page.SwimmingPace as SwimmingPacePage
import Elm.Page.SwimmingTime as SwimmingTimePage
import Elm.Util.Cmd as CmdUtil
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Process
import Task
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>))


type Route
    = NotFound
    | Home
    | RunningPace
    | RunningTime
    | RunningDistance
    | CyclingSpeed
    | CyclingTime
    | CyclingDistance
    | SwimmingPace
    | SwimmingTime
    | SwimmingDistance
    | Contact


type Content
    = NotFoundModel NotFoundPage.Model
    | HomeModel HomePage.Model
    | RunningPaceModel RunningPacePage.Model
    | RunningTimeModel RunningTimePage.Model
    | RunningDistanceModel RunningDistancePage.Model
    | CyclingSpeedModel CyclingSpeedPage.Model
    | CyclingTimeModel CyclingTimePage.Model
    | CyclingDistanceModel CyclingDistancePage.Model
    | SwimmingPaceModel SwimmingPacePage.Model
    | SwimmingTimeModel SwimmingTimePage.Model
    | SwimmingDistanceModel SwimmingDistancePage.Model
    | ContactModel ContactPage.Model


type alias Model =
    { nav : Bool
    , return : Bool
    , content : Content
    , snackbar : ( String, String )
    , visible : Bool
    }


type InternalMsg
    = OpenNav
    | CloseNav
    | NotFoundMsg NotFoundPage.Msg
    | HomeMsg HomePage.Msg
    | RunningPaceMsg RunningPacePage.Msg
    | RunningTimeMsg RunningTimePage.Msg
    | RunningDistanceMsg RunningDistancePage.Msg
    | CyclingSpeedMsg CyclingSpeedPage.Msg
    | CyclingTimeMsg CyclingTimePage.Msg
    | CyclingDistanceMsg CyclingDistancePage.Msg
    | SwimmingPaceMsg SwimmingPacePage.Msg
    | SwimmingTimeMsg SwimmingTimePage.Msg
    | SwimmingDistanceMsg SwimmingDistancePage.Msg
    | ContactMsg ContactPage.Msg
    | ShowSnackbar String
    | HideSnackbar
    | ShowContent


type ExternalMsg
    = ChangePage String


type Msg
    = Self InternalMsg
    | Parent ExternalMsg


init : Url -> ( Model, Cmd Msg )
init url =
    let
        route =
            UrlParser.parse parser url |> Maybe.withDefault NotFound

        ( runningPaceModel, runningPaceCmd ) =
            RunningPacePage.init

        ( runningTimeModel, runningTimeCmd ) =
            RunningTimePage.init

        ( runningDistanceModel, runningDistanceCmd ) =
            RunningDistancePage.init

        ( cyclingSpeedModel, cyclingSpeedCmd ) =
            CyclingSpeedPage.init

        ( cyclingTimeModel, cyclingTimeCmd ) =
            CyclingTimePage.init

        ( cyclingDistanceModel, cyclingDistanceCmd ) =
            CyclingDistancePage.init

        ( swimmingPaceModel, swimmingPaceCmd ) =
            SwimmingPacePage.init

        ( swimmingTimeModel, swimmingTimeCmd ) =
            SwimmingTimePage.init

        ( swimmingDistanceModel, swimmingDistanceCmd ) =
            SwimmingDistancePage.init
    in
    ( { nav = False
      , return = route /= Home
      , content =
            case route of
                NotFound ->
                    NotFoundModel NotFoundPage.init

                Home ->
                    HomeModel HomePage.init

                RunningPace ->
                    RunningPaceModel runningPaceModel

                RunningTime ->
                    RunningTimeModel runningTimeModel

                RunningDistance ->
                    RunningDistanceModel runningDistanceModel

                CyclingSpeed ->
                    CyclingSpeedModel cyclingSpeedModel

                CyclingTime ->
                    CyclingTimeModel cyclingTimeModel

                CyclingDistance ->
                    CyclingDistanceModel cyclingDistanceModel

                SwimmingPace ->
                    SwimmingPaceModel swimmingPaceModel

                SwimmingTime ->
                    SwimmingTimeModel swimmingTimeModel

                SwimmingDistance ->
                    SwimmingDistanceModel swimmingDistanceModel

                Contact ->
                    ContactModel ContactPage.init
      , snackbar = ( "false", "" )
      , visible = False
      }
    , Cmd.batch
        [ Process.sleep 100 |> Task.perform (\_ -> Self ShowContent)
        , case route of
            RunningPace ->
                Cmd.map (Self << RunningPaceMsg) runningPaceCmd

            RunningTime ->
                Cmd.map (Self << RunningTimeMsg) runningTimeCmd

            RunningDistance ->
                Cmd.map (Self << RunningDistanceMsg) runningDistanceCmd

            CyclingSpeed ->
                Cmd.map (Self << CyclingSpeedMsg) cyclingSpeedCmd

            CyclingTime ->
                Cmd.map (Self << CyclingTimeMsg) cyclingTimeCmd

            CyclingDistance ->
                Cmd.map (Self << CyclingDistanceMsg) cyclingDistanceCmd

            SwimmingPace ->
                Cmd.map (Self << SwimmingPaceMsg) swimmingPaceCmd

            SwimmingTime ->
                Cmd.map (Self << SwimmingTimeMsg) swimmingTimeCmd

            SwimmingDistance ->
                Cmd.map (Self << SwimmingDistanceMsg) swimmingDistanceCmd

            _ ->
                Cmd.none
        ]
    )


parser : UrlParser.Parser (Route -> a) a
parser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Home (UrlParser.s "index.html")
        , UrlParser.map RunningPace (UrlParser.s "running" </> UrlParser.s "pace")
        , UrlParser.map RunningTime (UrlParser.s "running" </> UrlParser.s "time")
        , UrlParser.map RunningDistance (UrlParser.s "running" </> UrlParser.s "distance")
        , UrlParser.map CyclingSpeed (UrlParser.s "cycling" </> UrlParser.s "speed")
        , UrlParser.map CyclingTime (UrlParser.s "cycling" </> UrlParser.s "time")
        , UrlParser.map CyclingDistance (UrlParser.s "cycling" </> UrlParser.s "distance")
        , UrlParser.map SwimmingPace (UrlParser.s "swimming" </> UrlParser.s "pace")
        , UrlParser.map SwimmingTime (UrlParser.s "swimming" </> UrlParser.s "time")
        , UrlParser.map SwimmingDistance (UrlParser.s "swimming" </> UrlParser.s "distance")
        , UrlParser.map Contact (UrlParser.s "contact")
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenNav ->
            ( { model | nav = True }
            , Cmd.none
            )

        CloseNav ->
            ( { model | nav = False }
            , Cmd.none
            )

        NotFoundMsg (NotFoundPage.Parent subMsg) ->
            ( model
            , case subMsg of
                NotFoundPage.GoHome ->
                    CmdUtil.fire (Parent << ChangePage <| "/")
            )

        HomeMsg subMsg ->
            case model.content of
                HomeModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            HomePage.update subMsg subModel
                    in
                    ( { model | content = HomeModel updatedModel }
                    , Cmd.map (Self << HomeMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        RunningPaceMsg (RunningPacePage.Self subMsg) ->
            case model.content of
                RunningPaceModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            RunningPacePage.update subMsg subModel
                    in
                    ( { model | content = RunningPaceModel updatedModel }
                    , Cmd.map (Self << RunningPaceMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        RunningPaceMsg (RunningPacePage.Parent subMsg) ->
            ( model
            , case subMsg of
                RunningPacePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                RunningPacePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        RunningTimeMsg (RunningTimePage.Self subMsg) ->
            case model.content of
                RunningTimeModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            RunningTimePage.update subMsg subModel
                    in
                    ( { model | content = RunningTimeModel updatedModel }
                    , Cmd.map (Self << RunningTimeMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        RunningTimeMsg (RunningTimePage.Parent subMsg) ->
            ( model
            , case subMsg of
                RunningTimePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                RunningTimePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        RunningDistanceMsg (RunningDistancePage.Self subMsg) ->
            case model.content of
                RunningDistanceModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            RunningDistancePage.update subMsg subModel
                    in
                    ( { model | content = RunningDistanceModel updatedModel }
                    , Cmd.map (Self << RunningDistanceMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        RunningDistanceMsg (RunningDistancePage.Parent subMsg) ->
            ( model
            , case subMsg of
                RunningDistancePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                RunningDistancePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        CyclingSpeedMsg (CyclingSpeedPage.Self subMsg) ->
            case model.content of
                CyclingSpeedModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            CyclingSpeedPage.update subMsg subModel
                    in
                    ( { model | content = CyclingSpeedModel updatedModel }
                    , Cmd.map (Self << CyclingSpeedMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CyclingSpeedMsg (CyclingSpeedPage.Parent subMsg) ->
            ( model
            , case subMsg of
                CyclingSpeedPage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                CyclingSpeedPage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        CyclingTimeMsg (CyclingTimePage.Self subMsg) ->
            case model.content of
                CyclingTimeModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            CyclingTimePage.update subMsg subModel
                    in
                    ( { model | content = CyclingTimeModel updatedModel }
                    , Cmd.map (Self << CyclingTimeMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CyclingTimeMsg (CyclingTimePage.Parent subMsg) ->
            ( model
            , case subMsg of
                CyclingTimePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                CyclingTimePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        CyclingDistanceMsg (CyclingDistancePage.Self subMsg) ->
            case model.content of
                CyclingDistanceModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            CyclingDistancePage.update subMsg subModel
                    in
                    ( { model | content = CyclingDistanceModel updatedModel }
                    , Cmd.map (Self << CyclingDistanceMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CyclingDistanceMsg (CyclingDistancePage.Parent subMsg) ->
            ( model
            , case subMsg of
                CyclingDistancePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                CyclingDistancePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        SwimmingPaceMsg (SwimmingPacePage.Self subMsg) ->
            case model.content of
                SwimmingPaceModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            SwimmingPacePage.update subMsg subModel
                    in
                    ( { model | content = SwimmingPaceModel updatedModel }
                    , Cmd.map (Self << SwimmingPaceMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        SwimmingPaceMsg (SwimmingPacePage.Parent subMsg) ->
            ( model
            , case subMsg of
                SwimmingPacePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                SwimmingPacePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        SwimmingTimeMsg (SwimmingTimePage.Self subMsg) ->
            case model.content of
                SwimmingTimeModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            SwimmingTimePage.update subMsg subModel
                    in
                    ( { model | content = SwimmingTimeModel updatedModel }
                    , Cmd.map (Self << SwimmingTimeMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        SwimmingTimeMsg (SwimmingTimePage.Parent subMsg) ->
            ( model
            , case subMsg of
                SwimmingTimePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                SwimmingTimePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        SwimmingDistanceMsg (SwimmingDistancePage.Self subMsg) ->
            case model.content of
                SwimmingDistanceModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            SwimmingDistancePage.update subMsg subModel
                    in
                    ( { model | content = SwimmingDistanceModel updatedModel }
                    , Cmd.map (Self << SwimmingDistanceMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        SwimmingDistanceMsg (SwimmingDistancePage.Parent subMsg) ->
            ( model
            , case subMsg of
                SwimmingDistancePage.ShowSnackbar message ->
                    CmdUtil.fire <| (Self << ShowSnackbar) message

                SwimmingDistancePage.HideSnackbar ->
                    CmdUtil.fire <| Self HideSnackbar
            )

        ContactMsg subMsg ->
            case model.content of
                ContactModel subModel ->
                    let
                        ( updatedModel, cmd ) =
                            ContactPage.update subMsg subModel
                    in
                    ( { model | content = ContactModel updatedModel }
                    , Cmd.map (Self << ContactMsg) cmd
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        ShowSnackbar message ->
            ( { model | snackbar = ( "true", message ) }
            , Cmd.none
            )

        HideSnackbar ->
            ( { model | snackbar = ( "false", Tuple.second model.snackbar ) }
            , Cmd.none
            )

        ShowContent ->
            ( { model | visible = True }
            , Cmd.none
            )


routeFromContent : Content -> Route
routeFromContent content =
    case content of
        NotFoundModel _ ->
            NotFound

        HomeModel _ ->
            Home

        RunningPaceModel _ ->
            RunningPace

        RunningTimeModel _ ->
            RunningTime

        RunningDistanceModel _ ->
            RunningDistance

        CyclingSpeedModel _ ->
            CyclingSpeed

        CyclingTimeModel _ ->
            CyclingTime

        CyclingDistanceModel _ ->
            CyclingDistance

        SwimmingPaceModel _ ->
            SwimmingPace

        SwimmingTimeModel _ ->
            SwimmingTime

        SwimmingDistanceModel _ ->
            SwimmingDistance

        ContactModel _ ->
            Contact


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.content of
        RunningPaceModel content ->
            Sub.map (Self << RunningPaceMsg) (RunningPacePage.subscriptions content)

        RunningTimeModel content ->
            Sub.map (Self << RunningTimeMsg) (RunningTimePage.subscriptions content)

        RunningDistanceModel content ->
            Sub.map (Self << RunningDistanceMsg) (RunningDistancePage.subscriptions content)

        CyclingSpeedModel content ->
            Sub.map (Self << CyclingSpeedMsg) (CyclingSpeedPage.subscriptions content)

        CyclingTimeModel content ->
            Sub.map (Self << CyclingTimeMsg) (CyclingTimePage.subscriptions content)

        CyclingDistanceModel content ->
            Sub.map (Self << CyclingDistanceMsg) (CyclingDistancePage.subscriptions content)

        SwimmingPaceModel content ->
            Sub.map (Self << SwimmingPaceMsg) (SwimmingPacePage.subscriptions content)

        SwimmingTimeModel content ->
            Sub.map (Self << SwimmingTimeMsg) (SwimmingTimePage.subscriptions content)

        SwimmingDistanceModel content ->
            Sub.map (Self << SwimmingDistanceMsg) (SwimmingDistancePage.subscriptions content)

        _ ->
            Sub.none


view : Model -> { title : String, body : Html Msg }
view model =
    let
        wrapper =
            div [ class "route mdc-typography" ]
    in
    case model.content of
        NotFoundModel subModel ->
            { title = "Speed Matters - 404"
            , body =
                wrapper
                    [ Html.map
                        (Self << NotFoundMsg)
                        (NotFoundPage.view subModel)
                    ]
            }

        HomeModel subModel ->
            { title = "Speed Matters - Home"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Speed Matters"
                        (Html.map
                            (Self << HomeMsg)
                            (HomePage.view subModel)
                        )
                    ]
            }

        RunningPaceModel subModel ->
            { title = "Speed Matters - Running"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Running pace"
                        (Html.map
                            (Self << RunningPaceMsg)
                            (RunningPacePage.view subModel)
                        )
                    ]
            }

        RunningTimeModel subModel ->
            { title = "Speed Matters - Running"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Running time"
                        (Html.map
                            (Self << RunningTimeMsg)
                            (RunningTimePage.view subModel)
                        )
                    ]
            }

        RunningDistanceModel subModel ->
            { title = "Speed Matters - Running"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Running distance"
                        (Html.map
                            (Self << RunningDistanceMsg)
                            (RunningDistancePage.view subModel)
                        )
                    ]
            }

        CyclingSpeedModel subModel ->
            { title = "Speed Matters - Cycling"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Cycling speed"
                        (Html.map
                            (Self << CyclingSpeedMsg)
                            (CyclingSpeedPage.view subModel)
                        )
                    ]
            }

        CyclingTimeModel subModel ->
            { title = "Speed Matters - Cycling"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Cycling time"
                        (Html.map
                            (Self << CyclingTimeMsg)
                            (CyclingTimePage.view subModel)
                        )
                    ]
            }

        CyclingDistanceModel subModel ->
            { title = "Speed Matters - Cycling"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Cycling distance"
                        (Html.map
                            (Self << CyclingDistanceMsg)
                            (CyclingDistancePage.view subModel)
                        )
                    ]
            }

        SwimmingPaceModel subModel ->
            { title = "Speed Matters - Swimming"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Swimming pace"
                        (Html.map
                            (Self << SwimmingPaceMsg)
                            (SwimmingPacePage.view subModel)
                        )
                    ]
            }

        SwimmingTimeModel subModel ->
            { title = "Speed Matters - Swimming"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Swimming time"
                        (Html.map
                            (Self << SwimmingTimeMsg)
                            (SwimmingTimePage.view subModel)
                        )
                    ]
            }

        SwimmingDistanceModel subModel ->
            { title = "Speed Matters - Swimming"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Swimming distance"
                        (Html.map
                            (Self << SwimmingDistanceMsg)
                            (SwimmingDistancePage.view subModel)
                        )
                    ]
            }

        ContactModel subModel ->
            { title = "Speed Matters - Contact"
            , body =
                wrapper
                    [ viewNav
                        model
                        "Contact"
                        (Html.map
                            (Self << ContactMsg)
                            (ContactPage.view subModel)
                        )
                    ]
            }


viewLink : String -> String -> String -> Bool -> Html Msg
viewLink path name icon isActive =
    a
        [ class "route__link"
        , classList [ ( "is-active", isActive ) ]
        , href path
        ]
        [ i [ class "material-icons" ]
            [ text icon ]
        , span [ href path ]
            [ text name ]
        ]


viewNav : Model -> String -> Html Msg -> Html Msg
viewNav { nav, return, snackbar, content, visible } title pageHtml =
    let
        currentRoute =
            routeFromContent content
    in
    node "mwc-drawer"
        ([ id "mwc-drawer"
         , attribute "hasheader" ""
         , type_ "modal"
         , on "MDCDrawer:closed" (Decode.succeed (Self CloseNav))
         ]
            ++ (case nav of
                    True ->
                        [ attribute "open" "" ]

                    False ->
                        []
               )
        )
        [ span [ attribute "slot" "title" ]
            [ text "Menu" ]
        , span [ attribute "slot" "subtitle" ]
            [ a
                [ href "/contact"
                , class "route__contact"
                ]
                [ text "Contact us" ]
            ]
        , div [ class "route__menu" ]
            [ div [ class "route__group" ]
                [ h4 [ class "route__group-title" ]
                    [ text "Running" ]
                , div [ class "route__group-links" ]
                    [ viewLink "/running/pace" "Pace" "timer" (currentRoute == RunningPace)
                    , viewLink "/running/time" "Time" "access_time" (currentRoute == RunningTime)
                    , viewLink "/running/distance" "Distance" "trending_flat" (currentRoute == RunningDistance)
                    ]
                ]
            , div [ class "route__divider" ] []
            , div [ class "route__group" ]
                [ h4 [ class "route__group-title" ]
                    [ text "Cycling" ]
                , div [ class "route__group-links" ]
                    [ viewLink "/cycling/speed" "Speed" "speed" False
                    , viewLink "/cycling/time" "Time" "access_time" False
                    , viewLink "/cycling/distance" "Distance" "trending_flat" False
                    ]
                ]
            , div [ class "route__divider" ] []
            , div [ class "route__group" ]
                [ h4 [ class "route__group-title" ]
                    [ text "Swimming" ]
                , div [ class "route__group-links" ]
                    [ viewLink "/swimming/pace" "Pace" "timer" (currentRoute == SwimmingPace)
                    , viewLink "/swimming/time" "Time" "access_time" (currentRoute == SwimmingTime)
                    , viewLink "/swimming/distance" "Distance" "trending_flat" (currentRoute == SwimmingDistance)
                    ]
                ]
            ]
        , div
            [ attribute "slot" "appContent"
            , class "route__wrapper"
            ]
            [ node "mwc-top-app-bar-fixed"
                []
                [ node "mwc-icon-button"
                    [ attribute "slot" "navigationIcon"
                    , attribute "icon" "menu"
                    , onClick <| Self OpenNav
                    ]
                    []
                , if return then
                    node "mwc-icon-button"
                        [ attribute "slot" "navigationIcon"
                        , attribute "icon" "arrow_back"
                        , onClick <| (Parent << ChangePage) "/"
                        ]
                        []

                  else
                    text ""
                , div [ attribute "slot" "title" ]
                    [ text title ]
                ]
            , div
                [ id "content"
                , class "route__content"
                ]
                [ div
                    [ class " route__page"
                    , classList [ ( "is-visible", visible ) ]
                    ]
                    [ pageHtml ]
                , footer [ class "route__footer mdc-typography--body2" ]
                    [ text "Copyright © 2019 Alex Pryshchepa. All right reserved."
                    , text " "
                    , span []
                        [ text "Icons made by"
                        , text " "
                        , a
                            [ href "https://www.flaticon.com/authors/freepik"
                            , attribute "title" "Freepik"
                            , target "_blank"
                            ]
                            [ text "Freepik" ]
                        , text " "
                        , text "from"
                        , text " "
                        , a
                            [ href "https://www.flaticon.com/"
                            , attribute "title" "Flaticon"
                            , target "_blank"
                            ]
                            [ text "www.flaticon.com" ]
                        ]
                    ]
                , node
                    "custom-mwc-snackbar"
                    [ attribute "timeoutMs" "4000"
                    , attribute "open" (Tuple.first snackbar)
                    , attribute "labeltext" (Tuple.second snackbar)
                    , on "MDCSnackbar:closed" (Decode.succeed <| Self HideSnackbar)
                    ]
                    []
                ]
            ]
        ]
