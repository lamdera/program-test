module Upgrade exposing (rule)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Elm.Writer
import Review.Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , changes : List Change
    , importRow : Int
    }


type alias Change =
    { fix : Fix
    , isCommand : Bool
    , isSubscription : Bool
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "SetupProgramTest" contextCreator
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    if List.isEmpty context.changes then
        []

    else
        let
            addCommandImport =
                if List.any .isCommand context.changes then
                    [ Review.Fix.insertAt
                        { row = context.importRow, column = 1 }
                        "import Effect.Command as Command exposing (Command)\n"
                    ]

                else
                    []

            addSubscriptionImport =
                if List.any .isSubscription context.changes then
                    [ Review.Fix.insertAt
                        { row = context.importRow, column = 1 }
                        "import Effect.Subscription as Subscription exposing (Subscription)\n"
                    ]

                else
                    []
        in
        [ Rule.errorWithFix
            { message = "This module needs upgrading"
            , details = [ "a" ]
            }
            { start = { column = 1, row = 1 }, end = { column = String.length "module" + 1, row = 1 } }
            (addSubscriptionImport ++ addCommandImport ++ List.map .fix context.changes)
        ]


contextCreator : Rule.ContextCreator () Context
contextCreator =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , changes = []
            , importRow = 1
            }
        )
        |> Rule.withModuleNameLookupTable


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor (Node range import_) context =
    ( []
    , { context
        | changes =
            upgradeModuleName (Node.range import_.moduleName) (Node.value import_.moduleName) ""
                ++ context.changes
        , importRow = range.start.row
      }
    )


declarationVisitor : Node Declaration -> Rule.Direction -> Context -> ( List (Error {}), Context )
declarationVisitor (Node _ declaration) direction context =
    case direction of
        Rule.OnEnter ->
            ( []
            , { context
                | changes =
                    (case declaration of
                        FunctionDeclaration function ->
                            functionVisitor context.lookupTable function

                        AliasDeclaration typeAlias ->
                            typeVisitor context.lookupTable typeAlias.typeAnnotation

                        CustomTypeDeclaration { constructors } ->
                            List.concatMap
                                (Node.value >> .arguments >> List.concatMap (typeVisitor context.lookupTable))
                                constructors

                        PortDeclaration _ ->
                            []

                        InfixDeclaration _ ->
                            []

                        Destructuring left right ->
                            destructureVisitor context.lookupTable left right
                    )
                        ++ context.changes
              }
            )

        Rule.OnExit ->
            ( [], context )


genericType : String -> Node TypeAnnotation
genericType =
    GenericType >> noRange


noRange =
    Node { start = { column = 0, row = 0 }, end = { column = 0, row = 0 } }


upgradeType : Range -> ModuleName -> String -> List (Node TypeAnnotation) -> List Change
upgradeType range moduleName name typeVars =
    let
        fixText =
            Elm.Writer.writeTypeAnnotation (noRange (Typed (noRange ( moduleName, name )) typeVars))
                |> Elm.Writer.write
    in
    [ { fix = Review.Fix.replaceRangeBy range fixText
      , isCommand = name == "Command"
      , isSubscription = name == "Subscription"
      }
    ]


typeVisitor : ModuleNameLookupTable -> Node TypeAnnotation -> List Change
typeVisitor lookupTable (Node range typeAnnotation) =
    case typeAnnotation of
        GenericType _ ->
            []

        Typed (Node range_ ( moduleName, typeName )) msgs ->
            let
                actualModuleName : ModuleName
                actualModuleName =
                    case Review.ModuleNameLookupTable.moduleNameAt lookupTable range_ of
                        Just moduleName_ ->
                            moduleName_

                        Nothing ->
                            moduleName
            in
            case ( actualModuleName, typeName, msgs ) of
                ( [ "Platform", "Cmd" ], "Cmd", [ msg ] ) ->
                    upgradeType range
                        []
                        "Command"
                        [ genericType "restriction", genericType "toMsg", msg ]

                ( [ "Platform", "Sub" ], "Sub", [ msg ] ) ->
                    upgradeType range [] "Subscription" [ genericType "restriction", msg ]

                ( [], "Task", [ x, a ] ) ->
                    upgradeType range [] "Task" [ genericType "restriction", x, a ]

                ( [ "Task" ], "Task", [ x, a ] ) ->
                    upgradeType range [ "Effect", "Task" ] "Task" [ genericType "restriction", x, a ]

                ( [ "Http" ], "Resolver", [ x, a ] ) ->
                    upgradeType range [ "Effect", "Http" ] "Resolver" [ genericType "restriction", x, a ]

                ( [ "Http" ], name, vars ) ->
                    upgradeType range [ "Effect", "Http" ] name vars

                ( [ "Browser", "Dom" ], name, vars ) ->
                    upgradeType range [ "Effect", "Browser", "Dom" ] name vars

                ( [ "Browser", "Events" ], name, vars ) ->
                    upgradeType range [ "Effect", "Browser", "Events" ] name vars

                ( [ "Browser", "Navigation" ], name, vars ) ->
                    upgradeType range [ "Effect", "Browser", "Navigation" ] name vars

                ( [ "Browser" ], name, vars ) ->
                    upgradeType range [ "Effect", "Browser" ] name vars

                ( [ "Time" ], name, vars ) ->
                    upgradeType range [ "Effect", "Time" ] name vars

                ( [ "File" ], name, vars ) ->
                    upgradeType range [ "Effect", "File" ] name vars

                ( _, _, nodes ) ->
                    List.concatMap (typeVisitor lookupTable) nodes

        Unit ->
            []

        Tupled nodes ->
            List.concatMap (typeVisitor lookupTable) nodes

        Record recordDefinition ->
            List.concatMap (Node.value >> Tuple.second >> typeVisitor lookupTable) recordDefinition

        GenericRecord _ (Node _ recordDefinition) ->
            List.concatMap (Node.value >> Tuple.second >> typeVisitor lookupTable) recordDefinition

        FunctionTypeAnnotation left right ->
            List.concatMap (typeVisitor lookupTable) [ left, right ]


functionVisitor : ModuleNameLookupTable -> Function -> List Change
functionVisitor lookupTable function =
    let
        { arguments, expression } =
            Node.value function.declaration
    in
    expressionVisitor lookupTable expression
        ++ List.concatMap (patternVisitor lookupTable) arguments
        ++ (case function.signature of
                Just (Node _ { typeAnnotation }) ->
                    typeVisitor lookupTable typeAnnotation

                Nothing ->
                    []
           )


destructureVisitor : ModuleNameLookupTable -> Node Pattern -> Node Expression -> List Change
destructureVisitor lookupTable left right =
    patternVisitor lookupTable left ++ expressionVisitor lookupTable right


letVisitor : ModuleNameLookupTable -> LetDeclaration -> List Change
letVisitor lookupTable letDeclaration =
    case letDeclaration of
        LetFunction function ->
            functionVisitor lookupTable function

        LetDestructuring left right ->
            destructureVisitor lookupTable left right


upgradeFunctionOrValue : Range -> Bool -> Bool -> List String -> Change
upgradeFunctionOrValue range isCommand isSubscription text =
    let
        fixText =
            List.filter ((/=) "") text |> String.join "."
    in
    { fix = Review.Fix.replaceRangeBy range fixText
    , isCommand = isCommand
    , isSubscription = isSubscription
    }


timeFunctions : Set.Set String
timeFunctions =
    Set.fromList
        [ "now"
        , "every"
        , "posixToMillis"
        , "millisToPosix"
        , "utc"
        , "here"
        , "toYear"
        , "toMonth"
        , "toDay"
        , "toWeekday"
        , "toHour"
        , "toMinute"
        , "toSecond"
        , "toMillis"
        , "customZone"
        , "getZoneName"
        ]


upgradeModuleName : Range -> ModuleName -> String -> List Change
upgradeModuleName range moduleName function =
    let
        error_ =
            [ upgradeFunctionOrValue range False False ("Effect" :: moduleName ++ [ function ]) ]
    in
    case moduleName of
        [ "Lamdera" ] ->
            error_

        [ "Browser" ] ->
            error_

        [ "Browser", "Dom" ] ->
            error_

        [ "Browser", "Events" ] ->
            error_

        [ "Browser", "Navigation" ] ->
            error_

        [ "File", "Download" ] ->
            error_

        [ "File", "Select" ] ->
            error_

        [ "File" ] ->
            error_

        [ "Http" ] ->
            error_

        [ "Process" ] ->
            error_

        [ "Task" ] ->
            error_

        [ "Time" ] ->
            error_

        [ "Platform", "Sub" ] ->
            [ upgradeFunctionOrValue range False True [ "Subscription", function ] ]

        [ "Platform", "Cmd" ] ->
            [ upgradeFunctionOrValue range True False [ "Command", function ] ]

        _ ->
            []


expressionVisitor : ModuleNameLookupTable -> Node Expression -> List Change
expressionVisitor lookupTable (Node range expression) =
    case expression of
        UnitExpr ->
            []

        Application nodes ->
            List.concatMap (expressionVisitor lookupTable) nodes

        OperatorApplication _ _ left right ->
            List.concatMap (expressionVisitor lookupTable) [ left, right ]

        FunctionOrValue moduleName function ->
            case Review.ModuleNameLookupTable.moduleNameAt lookupTable range of
                Just actualModuleName ->
                    upgradeModuleName range actualModuleName function

                Nothing ->
                    upgradeModuleName range moduleName function

        IfBlock condition ifTrue ifFalse ->
            List.concatMap (expressionVisitor lookupTable) [ condition, ifTrue, ifFalse ]

        PrefixOperator _ ->
            []

        Operator _ ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Negation _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        TupledExpression nodes ->
            List.concatMap (expressionVisitor lookupTable) nodes

        ParenthesizedExpression node ->
            expressionVisitor lookupTable node

        LetExpression letBlock ->
            expressionVisitor lookupTable letBlock.expression
                ++ List.concatMap (Node.value >> letVisitor lookupTable) letBlock.declarations

        CaseExpression caseBlock ->
            expressionVisitor lookupTable caseBlock.expression
                ++ List.concatMap (Tuple.second >> expressionVisitor lookupTable) caseBlock.cases
                ++ List.concatMap (Tuple.first >> patternVisitor lookupTable) caseBlock.cases

        LambdaExpression lambda ->
            expressionVisitor lookupTable lambda.expression ++ List.concatMap (patternVisitor lookupTable) lambda.args

        RecordExpr nodes ->
            List.concatMap (Node.value >> Tuple.second >> expressionVisitor lookupTable) nodes

        ListExpr nodes ->
            List.concatMap (expressionVisitor lookupTable) nodes

        RecordAccess node _ ->
            expressionVisitor lookupTable node

        RecordAccessFunction _ ->
            []

        RecordUpdateExpression _ nodes ->
            List.concatMap (Node.value >> Tuple.second >> expressionVisitor lookupTable) nodes

        GLSLExpression _ ->
            []


patternVisitor : ModuleNameLookupTable -> Node Pattern -> List Change
patternVisitor lookupTable (Node range pattern) =
    case pattern of
        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []

        TuplePattern nodes ->
            List.concatMap (patternVisitor lookupTable) nodes

        RecordPattern _ ->
            []

        UnConsPattern left right ->
            List.concatMap (patternVisitor lookupTable) [ left, right ]

        ListPattern nodes ->
            List.concatMap (patternVisitor lookupTable) nodes

        VarPattern _ ->
            []

        NamedPattern qualifiedNameRef nodes ->
            let
                offset =
                    String.join "." (qualifiedNameRef.moduleName ++ [ qualifiedNameRef.name ]) |> String.length
            in
            upgradeModuleName
                { start = range.start
                , end = { column = range.start.column + offset, row = range.start.row }
                }
                (case Review.ModuleNameLookupTable.moduleNameAt lookupTable range of
                    Just actualModuleName ->
                        actualModuleName

                    Nothing ->
                        qualifiedNameRef.moduleName
                )
                qualifiedNameRef.name
                ++ List.concatMap (patternVisitor lookupTable) nodes

        AsPattern node _ ->
            patternVisitor lookupTable node

        ParenthesizedPattern node ->
            patternVisitor lookupTable node
