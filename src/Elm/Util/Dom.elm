module Elm.Util.Dom exposing (scrollTop)

import Browser.Dom as Dom
import Task


scrollTop : String -> msg -> Cmd msg
scrollTop id tagger =
    Dom.getViewportOf id
        |> Task.andThen (\_ -> Dom.setViewportOf id 0 0)
        |> Task.attempt (\_ -> tagger)
