module Unwrap exposing (maybe, result)

{-| This library provides quick functions for uunwrapping value wrapper
types such as Maybe and Result types. I have found myself rewriting this time
and time again, so I thought to separate it out into a package.

Only use these if the program SHOULD crash in the case of the value not being
present.

# functions
@docs maybe, result
-}


{-| Forcibly unwraps a maybe. Crash the program when it is Nothing.

    Unwrap.maybe (Just 42) == 42
    Unwrap.maybe Nothing -- CRASHES
-}
maybe : Maybe a -> a
maybe maybeA =
    case maybeA of
        Just a ->
            a

        Nothing ->
            Debug.crash "Could not unwrap maybe"


{-| Forcibly unwraps a result. Crash the program when it is Err

    Unwrap.result (Ok 42) == 42
    Unwrap.result (Err "Should never happen") -- CRASHES
-}
result : Result err a -> a
result resultA =
    case resultA of
        Ok a ->
            a

        Err err ->
            Debug.crash ("Could not unwrap result. Error: " ++ (err |> toString))
