module Elm.Util.Cmd exposing (fire)

import Task


fire : msg -> Cmd msg
fire msg =
    Task.succeed msg |> Task.perform (\a -> a)
