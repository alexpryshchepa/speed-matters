module Main exposing
    ( Model
    , Msg(..)
    , init
    , main
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Elm.Route as Route
import Elm.Util.Cmd as CmdUtil
import Html exposing (..)
import Url


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route.Model
    }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RouteMsg Route.Msg
    | ChangePage Url.Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( routeModel, routeCmd ) =
            Route.init url
    in
    ( { key = key
      , url = url
      , route = routeModel
      }
    , Cmd.map RouteMsg routeCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            ( { model | url = url }
            , CmdUtil.fire <| ChangePage url
            )

        ChangePage url ->
            let
                ( routeModel, routeCmd ) =
                    Route.init url
            in
            ( { model | route = routeModel }
            , Cmd.map RouteMsg routeCmd
            )

        RouteMsg (Route.Self subMsg) ->
            let
                ( updatedModel, cmd ) =
                    Route.update subMsg model.route
            in
            ( { model | route = updatedModel }
            , Cmd.map RouteMsg cmd
            )

        RouteMsg (Route.Parent subMsg) ->
            ( model
            , case subMsg of
                Route.ChangePage url ->
                    Nav.pushUrl model.key url
            )


view : Model -> Browser.Document Msg
view model =
    let
        { title, body } =
            Route.view model.route
    in
    { title = title
    , body = [ Html.map RouteMsg body ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map RouteMsg (Route.subscriptions model.route)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
