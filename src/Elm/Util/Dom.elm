module Elm.Util.Dom exposing (resetViewport)

import Browser.Dom as Dom
import Task


resetViewport : msg -> Cmd msg
resetViewport tagger =
    Task.perform (\_ -> tagger) (Dom.setViewport 0 0)
