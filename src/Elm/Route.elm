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

import Elm.Page.Home as HomePage
import Elm.Page.NotFound as NotFoundPage
import Elm.Page.RunningPace as RunningPacePage
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


type Content
    = NotFoundModel NotFoundPage.Model
    | HomeModel HomePage.Model
    | RunningPaceModel RunningPacePage.Model


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
      , snackbar = ( "false", "" )
      , visible = False
      }
    , Cmd.batch
        [ Process.sleep 100 |> Task.perform (\_ -> Self ShowContent)
        , case route of
            RunningPace ->
                Cmd.map (Self << RunningPaceMsg) runningPaceCmd

            _ ->
                Cmd.none
        ]
    )


parser : UrlParser.Parser (Route -> a) a
parser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map RunningPace (UrlParser.s "running" </> UrlParser.s "pace")
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.content of
        RunningPaceModel content ->
            Sub.map (Self << RunningPaceMsg) (RunningPacePage.subscriptions content)

        _ ->
            Sub.none


view : Model -> { title : String, body : Html Msg }
view model =
    let
        wrapper =
            div [ class "route" ]
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
            [ text "Select calculator" ]
        , div [ class "route__menu" ]
            [ div [ class "route__group" ]
                [ h4 [ class "route__group-title" ]
                    [ text "Running" ]
                , div [ class "route__group-links" ]
                    [ viewLink "/running/pace" "Pace" "timer" (currentRoute == RunningPace)
                    , viewLink "/running/time" "Time" "access_time" False
                    , viewLink "/running/distance" "Distance" "trending_flat" False
                    ]
                ]
            , div [ class "route__divider" ] []
            , div [ class "route__group" ]
                [ h4 [ class "route__group-title" ]
                    [ text "Cycling" ]
                , div [ class "route__group-links" ]
                    [ viewLink "/cycling/power" "Estimated Power" "offline_bolt" False
                    , viewLink "/cycling/time" "Time" "access_time" False
                    , viewLink "/cycling/distance" "Distance" "trending_flat" False
                    , viewLink "/cycling/speed" "Speed" "speed" False
                    ]
                ]
            , div [ class "route__divider" ] []
            , div [ class "route__group" ]
                [ h4 [ class "route__group-title" ]
                    [ text "Swimming" ]
                , div [ class "route__group-links" ]
                    [ viewLink "/swimming/pace" "Pace" "timer" False
                    , viewLink "/swimming/time" "Time" "access_time" False
                    , viewLink "/swimming/distance" "Distance" "trending_flat" False
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
                    [ class "route__top-bar"
                    , attribute "slot" "navigationIcon"
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
                [ class "route__content" ]
                [ div
                    [ class " route__page"
                    , classList [ ( "is-visible", visible ) ]
                    ]
                    [ pageHtml ]
                , node
                    "custom-mwc-snackbar"
                    [ attribute "timeoutMs" "4000"
                    , attribute "open" (Tuple.first snackbar)
                    , attribute "labelText" (Tuple.second snackbar)
                    , on "MDCSnackbar:closed" (Decode.succeed <| Self HideSnackbar)
                    ]
                    []
                , footer [ class "route__footer" ]
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
                ]
            ]
        ]
