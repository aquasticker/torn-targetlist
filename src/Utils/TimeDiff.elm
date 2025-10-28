module Utils.TimeDiff exposing
    ( TimeDiff
    , asSeconds
    , between
    , formatAsClock
    , formatInHuman
    , inFuture
    )

import Time exposing (Posix)


type TimeDiff
    = TimeDiff Data


type alias Data =
    { future : Bool
    , diff : Int
    , days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    , milliseconds : Int
    , totalHours : Int
    }


asSeconds : TimeDiff -> Float
asSeconds (TimeDiff { diff }) =
    toFloat diff / 1000.0


between : Posix -> Posix -> TimeDiff
between t1 t2 =
    let
        diff : Int
        diff =
            Time.posixToMillis t1 - Time.posixToMillis t2

        future =
            diff > 0

        totalMillis =
            abs diff

        ( totalSeconds, milliseconds ) =
            splitParts 1000 totalMillis

        ( totalMinutes, seconds ) =
            splitParts 60 totalSeconds

        ( totalHours, minutes ) =
            splitParts 60 totalMinutes

        ( days, hours ) =
            splitParts 24 totalHours
    in
    TimeDiff
        { future = future
        , days = days
        , totalHours = totalHours
        , hours = hours
        , minutes = minutes
        , seconds = seconds
        , milliseconds = milliseconds
        , diff = diff
        }


inFuture : TimeDiff -> Bool
inFuture (TimeDiff data) =
    data.future


formatAsClock : TimeDiff -> String
formatAsClock (TimeDiff data) =
    let
        prefix =
            if data.future then
                ""

            else
                "- "
    in
    prefix
        ++ formatTimePart data.totalHours
        ++ ":"
        ++ formatTimePart data.minutes
        ++ ":"
        ++ formatTimePart data.seconds


formatTimePart : Int -> String
formatTimePart int =
    let
        str =
            String.fromInt int
    in
    if int < 10 then
        "0" ++ str

    else
        str


formatInHuman : TimeDiff -> String
formatInHuman (TimeDiff data) =
    if data.future then
        "in " ++ formatInHumanData data

    else
        formatInHumanData data ++ " ago"


formatInHumanData : Data -> String
formatInHumanData data =
    if data.days > 365 then
        formatYears data.days

    else if data.days > 30 then
        formatMonths data.days

    else if data.days > 0 then
        formatDays data.days

    else if data.hours > 0 then
        formatHours data.hours

    else if data.minutes > 0 then
        formatMinutes data.minutes

    else
        formatSeconds data.seconds


formatYears totalDays =
    let
        ( years, _ ) =
            splitParts 365 totalDays

        suffix =
            if years > 1 then
                " years"

            else
                " year"
    in
    String.fromInt years ++ suffix


formatMonths totalDays =
    let
        ( months, _ ) =
            splitParts 30 totalDays

        suffix =
            if months > 1 then
                " months"

            else
                " month"
    in
    String.fromInt months ++ suffix


formatDays totalDays =
    let
        suffix =
            if totalDays > 1 then
                " days"

            else
                " day"
    in
    String.fromInt totalDays ++ suffix


formatHours hours =
    let
        suffix =
            if hours > 1 then
                " hours"

            else
                " hour"
    in
    String.fromInt hours ++ suffix


formatMinutes minutes =
    let
        suffix =
            if minutes > 1 then
                " minutes"

            else
                " minute"
    in
    String.fromInt minutes ++ suffix


formatSeconds seconds =
    let
        suffix =
            if seconds > 1 then
                " seconds"

            else
                " second"
    in
    String.fromInt seconds ++ suffix


splitParts : Int -> Int -> ( Int, Int )
splitParts divider value =
    let
        mod =
            modBy divider value

        rest =
            floor (toFloat (value - mod) / toFloat divider)
    in
    ( rest, mod )
