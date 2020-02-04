module Elm.Service.Calculator exposing
    ( distance
    , distance2
    , hToMin
    , hToSec
    , kmPerHToMiPerH
    , kmToM
    , kmToMi
    , kmToYd
    , mToKm
    , mToMi
    , mToYd
    , miPerHToKmPerH
    , miToKm
    , miToM
    , miToYd
    , minToH
    , minToSec
    , pace
    , roundTo
    , secPerKmToSecPerMi
    , secPerMToSecPerYd
    , secPerMiToSecPerKm
    , secPerYdToSecPerM
    , secToH
    , secToMin
    , speed
    , time
    , time2
    , ydToKm
    , ydToM
    , ydToMi
    )


roundTo : Int -> Float -> Float
roundTo to num =
    (num * toFloat to
        |> round
        |> toFloat
    )
        / toFloat to



-- Distance


kmToMi : Float -> Float
kmToMi km =
    km / 1.60934
        |> roundTo 1000


kmToM : Float -> Int
kmToM km =
    round <| km * 1000


kmToYd : Float -> Int
kmToYd km =
    round <| km * 1093.613


mToMi : Int -> Float
mToMi m =
    toFloat m / 1609.34
        |> roundTo 1000


mToKm : Int -> Float
mToKm m =
    toFloat m / 1000


mToYd : Int -> Int
mToYd m =
    round <| toFloat m * 1.093613


miToKm : Float -> Float
miToKm mi =
    mi * 1.60934
        |> roundTo 1000


miToM : Float -> Int
miToM mi =
    round <| mi * 1609.34


miToYd : Float -> Int
miToYd mi =
    round <| mi * 1760


ydToMi : Int -> Float
ydToMi yd =
    toFloat yd / 1760
        |> roundTo 1000


ydToKm : Int -> Float
ydToKm yd =
    toFloat yd / 1093.613
        |> roundTo 1000


ydToM : Int -> Int
ydToM yd =
    round <| toFloat yd / 1.093613


distance : Int -> Int -> Float
distance timeSec paceSec =
    if paceSec <= 0 then
        0

    else
        toFloat timeSec / toFloat paceSec
            |> roundTo 1000


distance2 : Float -> Float -> Float
distance2 t s =
    t * s
        |> roundTo 1000



-- Time


hToMin : Int -> Int
hToMin =
    (*) 60


hToSec : Int -> Int
hToSec =
    (*) 3600


minToH : Float -> Float
minToH min =
    min / 60


minToSec : Int -> Int
minToSec =
    (*) 60


secToH : Int -> Float
secToH sec =
    toFloat sec / 3600


secToMin : Int -> Float
secToMin sec =
    toFloat sec / 60


time : Float -> Int -> Int
time d sec =
    floor <| d * toFloat sec


time2 : Float -> Float -> Float
time2 d s =
    d / s



-- Pace


secPerKmToSecPerMi : Int -> Int
secPerKmToSecPerMi sec =
    round <| toFloat sec * 1.60934


secPerMiToSecPerKm : Int -> Int
secPerMiToSecPerKm sec =
    round <| toFloat sec / 1.60934


secPerYdToSecPerM : Int -> Int
secPerYdToSecPerM sec =
    round <| toFloat sec * 1.093613


secPerMToSecPerYd : Int -> Int
secPerMToSecPerYd sec =
    round <| toFloat sec / 1.093613


pace : Int -> Float -> Int
pace sec d =
    if d <= 0 then
        0

    else
        floor <| toFloat sec / d



-- Speed


kmPerHToMiPerH : Float -> Float
kmPerHToMiPerH kmh =
    kmh / 1.60934
        |> roundTo 10


miPerHToKmPerH : Float -> Float
miPerHToKmPerH mph =
    mph * 1.60934
        |> roundTo 10


speed : Float -> Float -> Float
speed d t =
    if t <= 0 then
        0

    else
        d / t
            |> roundTo 10
