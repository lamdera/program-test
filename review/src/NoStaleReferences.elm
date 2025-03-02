module NoStaleReferences exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import List.Extra
import Parser exposing ((|=))
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator
        "NoStaleReferences"
        (Rule.initContextCreator (\() -> { inScope = Dict.empty }))
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor declaration context =
    case Node.value declaration of
        FunctionDeclaration function ->
            let
                declaration2 =
                    Node.value function.declaration
            in
            ( expressionVisitor
                (List.foldl (\arg dict -> mergeVars (patternVisitor arg) dict) Dict.empty declaration2.arguments)
                declaration2.expression
            , context
            )

        AliasDeclaration typeAlias ->
            ( [], context )

        CustomTypeDeclaration type_ ->
            ( [], context )

        PortDeclaration signature ->
            ( [], context )

        InfixDeclaration infix_ ->
            ( [], context )

        Destructuring pattern expression ->
            ( [], context )


expressionVisitor : Dict String VarData -> Node Expression -> List (Rule.Error {})
expressionVisitor inScope expression =
    case Node.value expression of
        UnitExpr ->
            []

        Application nodes ->
            List.concatMap (expressionVisitor inScope) nodes

        OperatorApplication _ _ left right ->
            expressionVisitor inScope left ++ expressionVisitor inScope right

        FunctionOrValue moduleName name ->
            case ( moduleName, String.left 1 name == String.toLower (String.left 1 name) ) of
                ( [], True ) ->
                    isLatestReference (Node (Node.range expression) name) inScope

                _ ->
                    []

        IfBlock condition ifTrue ifFalse ->
            expressionVisitor inScope condition
                ++ expressionVisitor inScope ifTrue
                ++ expressionVisitor inScope ifFalse

        PrefixOperator _ ->
            []

        Operator string ->
            []

        Integer int ->
            []

        Hex int ->
            []

        Floatable float ->
            []

        Negation node ->
            expressionVisitor inScope node

        Literal string ->
            []

        CharLiteral char ->
            []

        TupledExpression nodes ->
            List.concatMap (expressionVisitor inScope) nodes

        ParenthesizedExpression node ->
            expressionVisitor inScope node

        LetExpression letBlock ->
            let
                ( inScope3, errors2 ) =
                    List.foldl
                        (\(Node _ a) ( inScope2, errors ) ->
                            case a of
                                LetFunction function ->
                                    let
                                        name : Node String
                                        name =
                                            Node.value function.declaration |> .name
                                    in
                                    ( insertVar name inScope2
                                    , checkOrder name inScope2
                                        ++ expressionVisitor inScope2 (Node.value function.declaration).expression
                                        ++ errors
                                    )

                                LetDestructuring pattern b ->
                                    let
                                        newVars : Dict String VarData
                                        newVars =
                                            patternVisitor pattern
                                    in
                                    ( mergeVars newVars inScope2
                                    , List.concatMap
                                        (\( _, newVarData ) -> checkOrder newVarData.original inScope2)
                                        (Dict.toList newVars)
                                        ++ expressionVisitor inScope2 b
                                        ++ errors
                                    )
                        )
                        ( inScope, [] )
                        letBlock.declarations
            in
            expressionVisitor inScope3 letBlock.expression ++ errors2

        CaseExpression caseBlock ->
            expressionVisitor inScope caseBlock.expression
                ++ List.concatMap
                    (\( pattern, expression2 ) ->
                        expressionVisitor (mergeVars (patternVisitor pattern) inScope) expression2
                    )
                    caseBlock.cases

        LambdaExpression lambda ->
            expressionVisitor
                (List.foldl (\arg dict -> mergeVars dict (patternVisitor arg)) inScope lambda.args)
                lambda.expression

        RecordExpr nodes ->
            List.concatMap
                (\(Node _ ( _, expression2 )) ->
                    expressionVisitor inScope expression2
                )
                nodes

        ListExpr nodes ->
            List.concatMap (expressionVisitor inScope) nodes

        RecordAccess node _ ->
            expressionVisitor inScope node

        RecordAccessFunction string ->
            []

        RecordUpdateExpression _ nodes ->
            List.concatMap
                (\(Node _ ( _, expression2 )) ->
                    expressionVisitor inScope expression2
                )
                nodes

        GLSLExpression string ->
            []


checkOrder name inScope2 =
    let
        versioned : { name : String, version : Int }
        versioned =
            versionedVar (Node.value name)
    in
    case Dict.get versioned.name inScope2 of
        Just previous ->
            if previous.version > versioned.version then
                [ Rule.error
                    { message =
                        "Variables are defined out of order. You've defined "
                            ++ Node.value name
                            ++ " here but "
                            ++ versioned.name
                            ++ String.fromInt previous.version
                            ++ " is already defined. Reorder them so "
                            ++ Node.value name
                            ++ " is first."
                    , details =
                        [ "It's confusing when variables are defined out of order. For example"
                        , """    let
        a1 = a0 * 2
        a0 = 5 -- We're going bottom to top here which is confusing
    in
    a1"""
                        , "versus"
                        , """    let
        a0 = 5 -- This reads more naturally
        a1 = a0 * 2
    in
    a1"""
                        ]
                    }
                    (Node.range name)
                ]

            else
                []

        Nothing ->
            []


type alias VarData =
    { version : Int, original : Node String }


patternVisitor : Node Pattern -> Dict String VarData
patternVisitor pattern =
    case Node.value pattern of
        UnitPattern ->
            Dict.empty

        AllPattern ->
            Dict.empty

        CharPattern char ->
            Dict.empty

        StringPattern string ->
            Dict.empty

        IntPattern int ->
            Dict.empty

        HexPattern int ->
            Dict.empty

        FloatPattern float ->
            Dict.empty

        TuplePattern nodes ->
            List.map patternVisitor nodes |> List.foldl mergeVars Dict.empty

        RecordPattern nodes ->
            List.foldl insertVar Dict.empty nodes

        UnConsPattern left right ->
            mergeVars (patternVisitor right) (patternVisitor left)

        ListPattern nodes ->
            List.map patternVisitor nodes |> List.foldl mergeVars Dict.empty

        VarPattern name ->
            insertVar (Node (Node.range pattern) name) Dict.empty

        NamedPattern _ nodes ->
            List.map patternVisitor nodes |> List.foldl mergeVars Dict.empty

        AsPattern node name ->
            insertVar name (patternVisitor node)

        ParenthesizedPattern node ->
            patternVisitor node


mergeVars : Dict String VarData -> Dict String VarData -> Dict String VarData
mergeVars dictA dictB =
    Dict.foldl
        (\name varData dict ->
            Dict.update name
                (\maybe ->
                    case maybe of
                        Just a ->
                            if a.version > varData.version then
                                Just a

                            else
                                Just varData

                        Nothing ->
                            Just varData
                )
                dict
        )
        dictA
        dictB


insertVar : Node String -> Dict String VarData -> Dict String VarData
insertVar name dict =
    let
        versioned =
            versionedVar (Node.value name)
    in
    Dict.update
        versioned.name
        (\maybe ->
            { version =
                case maybe of
                    Just v ->
                        max versioned.version v.version

                    Nothing ->
                        versioned.version
            , original = name
            }
                |> Just
        )
        dict


isLatestReference : Node String -> Dict String VarData -> List (Rule.Error {})
isLatestReference name inScope =
    let
        versioned =
            versionedVar (Node.value name)
    in
    case Dict.get versioned.name inScope of
        Just { version } ->
            if version > versioned.version then
                [ Rule.error
                    { message =
                        Node.value name
                            ++ " is a stale reference. "
                            ++ versioned.name
                            ++ String.fromInt version
                            ++ " is the latest version, use it instead."
                    , details =
                        [ "If you have multiple instances of the same value, for example:"
                        , """    update model =
        let
            model2 = doStuff model
            model3 = doMoreStuff model
        in
        model3"""
                        , "It's easy to accidentally use the wrong version and end up with subtle bugs. Did you notice I forgot to use model2?"
                        , """In cases where you need to reference multiple versions of a variable at the same time, use a different naming scheme such as:
• varA, varB, varC
• varOld and varNew
• Use more descriptive names that don't require a suffix on the end to distinguish them
• Write `oldModel = model` and then use that where it's needed. This makes it clear that you're intentionally using the stale reference and it's not a mistake.
"""
                        ]
                    }
                    (Node.range name)
                ]

            else
                []

        Nothing ->
            []


versionedVar : String -> { name : String, version : Int }
versionedVar name =
    case String.toList name |> List.reverse |> List.Extra.splitWhen (\char -> not (Char.isDigit char)) of
        Just ( before, after ) ->
            case String.fromList (List.reverse before) |> String.toInt of
                Just version ->
                    { name = String.fromList (List.reverse after)
                    , version = version
                    }

                Nothing ->
                    { name = name
                    , version = 0
                    }

        Nothing ->
            { name = name
            , version = 0
            }


type alias Context =
    { inScope : Dict String VarData }
