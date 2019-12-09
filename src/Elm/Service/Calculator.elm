module Elm.Service.Calculator exposing
    ( hToMin
    , hToSec
    , kmToM
    , kmToMi
    , kmToYd
    , mToKm
    , mToMi
    , mToYd
    , miToKm
    , miToM
    , miToYd
    , minToH
    , minToSec
    , pace
    , secPerKmToSecPerMi
    , secPerMiToSecPerKm
    , secToH
    , secToMin
    , ydToKm
    , ydToM
    , ydToMi
    )


roundTo : Int -> Float -> Float
roundTo to num =
    ((*) num (toFloat to)
        |> round
        |> toFloat
    )
        / toFloat to



-- Distance


kmToMi : Float -> Float
kmToMi km =
    (/) km 1.60934
        |> roundTo 1000


kmToM : Float -> Int
kmToM km =
    round <| (*) km 1000


kmToYd : Float -> Int
kmToYd km =
    round <| (*) km 1093.613


mToMi : Int -> Float
mToMi m =
    (/) (toFloat m) 1609.34
        |> roundTo 1000


mToKm : Int -> Float
mToKm m =
    (/) (toFloat m) 1000


mToYd : Int -> Int
mToYd m =
    round <| (*) (toFloat m) 1.093613


miToKm : Float -> Float
miToKm mi =
    (*) mi 1.60934
        |> roundTo 1000


miToM : Float -> Int
miToM mi =
    round <| (*) mi 1609.34


miToYd : Float -> Int
miToYd mi =
    round <| (*) mi 1760


ydToMi : Int -> Float
ydToMi yd =
    (/) (toFloat yd) 1760
        |> roundTo 1000


ydToKm : Int -> Float
ydToKm yd =
    (/) (toFloat yd) 1093.613
        |> roundTo 1000


ydToM : Int -> Int
ydToM yd =
    round <| (/) (toFloat yd) 1.093613



-- Time


hToMin : Int -> Int
hToMin =
    (*) 60


hToSec : Int -> Int
hToSec =
    (*) 3600


minToH : Int -> Int
minToH min =
    (//) min 60


minToSec : Int -> Int
minToSec =
    (*) 60


secToH : Int -> Int
secToH sec =
    (//) sec 3600


secToMin : Int -> Int
secToMin sec =
    (//) sec 60



-- Pace


secPerKmToSecPerMi : Int -> Int
secPerKmToSecPerMi sec =
    round <| (*) (toFloat sec) 1.60934


secPerMiToSecPerKm : Int -> Int
secPerMiToSecPerKm sec =
    round <| (/) (toFloat sec) 1.60934


pace : Int -> Float -> Int
pace sec distance =
    floor <| (/) (toFloat sec) distance
