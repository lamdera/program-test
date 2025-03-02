module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.ReviewAtDocs
import NoBrokenParserFunctions
import NoConfusingPrefixOperator
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeConstructor
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoSimpleLetBody
import NoStaleReferences
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes
import Simplify


config : List Rule
config =
    [ NoStaleReferences.rule
    , NoUnused.Patterns.rule
    , Docs.ReviewAtDocs.rule
    , NoConfusingPrefixOperator.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , ReviewPipelineStyles.rule
        [ ReviewPipelineStyles.forbid ReviewPipelineStyles.leftPizzaPipelines
            |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToParentheticalApplication
            |> ReviewPipelineStyles.andCallThem "forbidden <| pipeline"
        , ReviewPipelineStyles.forbid ReviewPipelineStyles.leftCompositionPipelines
            |> ReviewPipelineStyles.andCallThem "forbidden << composition"
        , ReviewPipelineStyles.forbid ReviewPipelineStyles.rightCompositionPipelines
            |> ReviewPipelineStyles.andCallThem "forbidden >> composition"
        ]
    , Simplify.rule Simplify.defaults
    , NoMissingTypeConstructor.rule
    , NoBrokenParserFunctions.rule
    ]
        |> List.map
            (\rule ->
                rule
                    |> Review.Rule.ignoreErrorsForDirectories
                        [ "src/Effect/Browser"
                        , "src/Effect/File"
                        , "src/Effect/Time"
                        , "src/Effect/WebGL"
                        , "src/Test"
                        , "src/WebGLFix"
                        ]
                    |> Review.Rule.ignoreErrorsForFiles
                        [ "src/Effect/Command.elm"
                        , "src/Effect/File.elm"
                        , "src/Effect/Http.elm"
                        , "src/Effect/Internal.elm"
                        , "src/Effect/Lamdera.elm"
                        , "src/Effect/Process.elm"
                        , "src/Effect/Snapshot.elm"
                        , "src/Effect/Subscription.elm"
                        , "src/Effect/Task.elm"
                        , "src/Effect/Time.elm"
                        , "src/Effect/WebGL.elm"
                        , "src/WebGLFix.elm"
                        ]
            )
