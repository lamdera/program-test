module Effect.Task exposing
    ( Task
    , andThen
    , fail
    , getElement
    , getTime
    , getTimeZone
    , getTimeZoneName
    , getViewport
    , map
    , map2
    , map3
    , map4
    , map5
    , mapError
    , onError
    , setViewport
    , succeed
    , wait
    )

import Browser.Dom
import Duration exposing (Duration)
import Effect.Internal exposing (Effect(..), HttpBody(..), Task(..))
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Time


type alias FrontendOnly =
    Effect.Internal.FrontendOnly


type alias BackendOnly =
    Effect.Internal.BackendOnly


type alias Effect restriction toMsg msg =
    Effect.Internal.Effect restriction toMsg msg


type alias Task restriction x a =
    Effect.Internal.Task restriction x a


{-| -}
perform : (a -> msg) -> Task restriction Never a -> Effect restriction toMsg msg
perform f task =
    task
        |> map f
        |> mapError never
        |> Task


{-| This is very similar to [`perform`](#perform) except it can handle failures!
-}
attempt : (Result x a -> msg) -> Task restriction x a -> Effect restriction toMsg msg
attempt f task =
    task
        |> map (Ok >> f)
        |> mapError (Err >> f)
        |> Task


getTime : Task restriction x Time.Posix
getTime =
    GetTime Succeed


wait : Duration -> Task restriction x ()
wait duration =
    SleepTask duration Succeed


getTimeZone : Task FrontendOnly x Time.Zone
getTimeZone =
    GetTimeZone Succeed


getTimeZoneName : Task FrontendOnly x Time.ZoneName
getTimeZoneName =
    GetTimeZoneName Succeed


setViewport : Quantity Float Pixels -> Quantity Float Pixels -> Task FrontendOnly x ()
setViewport x y =
    SetViewport x y Succeed


getViewport : Task FrontendOnly x Browser.Dom.Viewport
getViewport =
    GetViewport Succeed


getElement : String -> Task FrontendOnly Browser.Dom.Error Browser.Dom.Element
getElement htmlId =
    GetElement
        (\result ->
            case result of
                Ok ok ->
                    Succeed ok

                Err err ->
                    Fail err
        )
        htmlId


{-| Chain together a task and a callback.
-}
andThen : (a -> Task restriction x b) -> Task restriction x a -> Task restriction x b
andThen f task =
    case task of
        Succeed a ->
            f a

        Fail x ->
            Fail x

        HttpTask request ->
            HttpTask
                { method = request.method
                , url = request.url
                , body = request.body
                , headers = request.headers
                , onRequestComplete = request.onRequestComplete >> andThen f
                , timeout = request.timeout
                }

        SleepTask delay onResult ->
            SleepTask delay (onResult >> andThen f)

        GetTime gotTime ->
            GetTime (gotTime >> andThen f)

        GetTimeZone gotTimeZone ->
            GetTimeZone (gotTimeZone >> andThen f)

        GetTimeZoneName gotTimeZoneName ->
            GetTimeZoneName (gotTimeZoneName >> andThen f)

        SetViewport x y function ->
            SetViewport x y (function >> andThen f)

        GetViewport function ->
            GetViewport (function >> andThen f)

        GetElement function string ->
            GetElement (function >> andThen f) string

        FileToString file function ->
            FileToString file (function >> andThen f)

        FileToBytes file function ->
            FileToBytes file (function >> andThen f)

        FileToUrl file function ->
            FileToUrl file (function >> andThen f)


{-| A task that succeeds immediately when run.
-}
succeed : a -> Task restriction x a
succeed =
    Succeed


{-| A task that fails immediately when run.
-}
fail : x -> Task restriction x a
fail =
    Fail


{-| Transform a task.
-}
map : (a -> b) -> Task restriction x a -> Task restriction x b
map f =
    andThen (f >> Succeed)


{-| Put the results of two tasks together.
-}
map2 : (a -> b -> result) -> Task restriction x a -> Task restriction x b -> Task restriction x result
map2 func taskA taskB =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen (\b -> succeed (func a b))
            )


{-| Put the results of three tasks together.
-}
map3 : (a -> b -> c -> result) -> Task restriction x a -> Task restriction x b -> Task restriction x c -> Task restriction x result
map3 func taskA taskB taskC =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen (\c -> succeed (func a b c))
                        )
            )


{-| Put the results of four tasks together.
-}
map4 :
    (a -> b -> c -> d -> result)
    -> Task restriction x a
    -> Task restriction x b
    -> Task restriction x c
    -> Task restriction x d
    -> Task restriction x result
map4 func taskA taskB taskC taskD =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen (\d -> succeed (func a b c d))
                                    )
                        )
            )


{-| Put the results of five tasks together.
-}
map5 :
    (a -> b -> c -> d -> e -> result)
    -> Task restriction x a
    -> Task restriction x b
    -> Task restriction x c
    -> Task restriction x d
    -> Task restriction x e
    -> Task restriction x result
map5 func taskA taskB taskC taskD taskE =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen (\e -> succeed (func a b c d e))
                                                )
                                    )
                        )
            )


{-| Transform the error value.
-}
mapError : (x -> y) -> Task restriction x a -> Task restriction y a
mapError f task =
    case task of
        Succeed a ->
            Succeed a

        Fail x ->
            Fail (f x)

        HttpTask request ->
            HttpTask
                { method = request.method
                , url = request.url
                , body = request.body
                , headers = request.headers
                , onRequestComplete = request.onRequestComplete >> mapError f
                , timeout = request.timeout
                }

        SleepTask delay onResult ->
            SleepTask delay (onResult >> mapError f)

        GetTime gotTime ->
            GetTime (gotTime >> mapError f)

        GetTimeZone gotTimeZone ->
            GetTimeZone (gotTimeZone >> mapError f)

        GetTimeZoneName gotTimeZoneName ->
            GetTimeZoneName (gotTimeZoneName >> mapError f)

        SetViewport x y function ->
            SetViewport x y (function >> mapError f)

        GetViewport function ->
            GetViewport (function >> mapError f)

        GetElement function string ->
            GetElement (function >> mapError f) string

        FileToString file function ->
            FileToString file (function >> mapError f)

        FileToBytes file function ->
            FileToBytes file (function >> mapError f)

        FileToUrl file function ->
            FileToUrl file (function >> mapError f)


{-| Recover from a failure in a task.
-}
onError : (x -> Task restriction y a) -> Task restriction x a -> Task restriction y a
onError f task =
    case task of
        Succeed a ->
            Succeed a

        Fail x ->
            f x

        HttpTask request ->
            HttpTask
                { method = request.method
                , url = request.url
                , body = request.body
                , headers = request.headers
                , onRequestComplete = request.onRequestComplete >> onError f
                , timeout = request.timeout
                }

        SleepTask delay onResult ->
            SleepTask delay (onResult >> onError f)

        GetTime gotTime ->
            GetTime (gotTime >> onError f)

        GetTimeZone gotTimeZone ->
            GetTimeZone (gotTimeZone >> onError f)

        GetTimeZoneName gotTimeZoneName ->
            GetTimeZoneName (gotTimeZoneName >> onError f)

        SetViewport x y function ->
            SetViewport x y (function >> onError f)

        GetViewport function ->
            GetViewport (function >> onError f)

        GetElement function string ->
            GetElement (function >> onError f) string

        FileToString file function ->
            FileToString file (function >> onError f)

        FileToBytes file function ->
            FileToBytes file (function >> onError f)

        FileToUrl file function ->
            FileToUrl file (function >> onError f)
