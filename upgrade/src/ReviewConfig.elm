module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule exposing (Rule)
import Upgrade


ignoredDirs : List String
ignoredDirs =
    [ "tests/", "vendor/", "src/Evergreen/", ".elm-spa/" ]

ignoredFiles : List String
ignoredFiles =
    [ "src/Config.elm", "src/Env.elm" ]

config : List Rule
config =
    -- Note: Don't apply elm-review to vendored code, tests, elm-spa generated code, Evergreen, and a few special
    --       Lamdera-related Elm files
    config_
        |> List.map (\rule -> rule
        |> Rule.ignoreErrorsForDirectories ignoredDirs
        |> Rule.ignoreErrorsForFiles ignoredFiles
        )
