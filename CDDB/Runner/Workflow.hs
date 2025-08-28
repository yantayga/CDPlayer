{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner.Workflow  where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Either (rights)

import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)

import CDDB.CDDB
import CDDB.Templates
import CDDB.Rules
import CDDB.AddFact
import CDDB.Logging
import CDDB.DeleteNodes
import CDDB.Utils
import CDDB.Expression.VariableDefs
import CDDB.Expression.Eval
import CDDB.Expression.Constants
import CDDB.Tree.Syntax
import CDDB.Runner.Context

type WorkFlowResult = Either [Context] [Context]

type WorkflowCommand = Rule -> Context -> WorkFlowResult

ruleWorkflow :: [WorkflowCommand]
ruleWorkflow = [
    bindLocalVariables,
    checkRuleConditions,
    runParent,
    addFacts,
    deleteTreeNodes,
    checkStop
    ]

applyTree :: Context -> SyntacticTree -> [Context]
applyTree ctx t = applyTreeWithContext $ ctx {currentTree = t}

applyTreeWithContext :: Context -> [Context]
applyTreeWithContext ctx = finished ++ maxdepth ++ concatMap subApply shallow
    where
        (finished, nonfinished) = span ((== Finished) . state) $ applyTreeOnce ctx
        (shallow, maxdepth) = span ((< contextMaxRecursionDepth ctx) . recursionDepth) nonfinished
        subApply sctx = applyTreeWithContext (resetContext sctx)

applyTreeOnce :: Context-> [Context]
applyTreeOnce ctx = concat $ rights $ map evaluateRuleAt $ matchRules (currentTree ctx) (rules (db ctx))
    where
        evaluateRuleAt (r, vs) = runWorkflow r $ ctx {variableStates = vs}

matchRules :: SyntacticTree -> Rules -> [(Rule, VariableStates)]
matchRules t rules = M.elems $ matchRulesAndFindPaths t rules

runWorkflow :: Rule -> Context -> WorkFlowResult
runWorkflow rule ctx = applySteps ctx $ map ($ rule) ruleWorkflow

applySteps :: Context -> [Context -> WorkFlowResult] -> WorkFlowResult
applySteps ctx = foldl runStep (Right [ctx])
    where
        runStep :: WorkFlowResult -> (Context -> WorkFlowResult) -> WorkFlowResult
        runStep (Left cs) _ = Left cs
        runStep (Right cs) f = concatMapM f cs

bindLocalVariables :: WorkflowCommand
bindLocalVariables rule ctx = ok $ ctx {variableStates = foldl addVariableDef (variableStates ctx) (locals rule)}

checkRuleConditions :: WorkflowCommand
checkRuleConditions rule ctx = if all ((== CBoolean True) . evaluateExpression (variableStates ctx)) (conditions rule)
    then debug "Rule passed conditions." ctx
    else err "Rule has not passed conditions." ctx

runParent :: WorkflowCommand
runParent rule ctx = Right $ applyTree ctx $ bindTreeVariables (variableStates ctx) (parent rule)

addFacts :: WorkflowCommand
addFacts rule ctx = applySteps ctx $ map addOneFact (facts rule)

addOneFact :: AddFact -> Context -> WorkFlowResult
addOneFact (AddFact name variableDefs) ctx = case findTemplate (templates $ db ctx) name of
    Nothing -> err ("Add fact failed: template " ++ name ++ "not found.") ctx
    Just template -> addOneFact' template variableDefs ctx

addOneFact' :: PrimitiveTemplate -> VariableDefs -> Context -> WorkFlowResult
addOneFact' (PrimitiveTemplate name fieldNames) variableDefs ctx =  if Nothing `elem` maybeFs
    then err ("Add fact failed: variables " ++ show notFound ++ " not found in template" ++ name ++ ".") ctx
    else ok $ ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) (catMaybes maybeFs): accumulatedKnowledge ctx}
    where
        evaluateFact states fieldVariables = Fact name $ map (evaluateExpression states) fieldVariables
        maybeFs = map (flip lookup $ map unpackDef variableDefs) fieldNames
        unpackDef (VariableDef n ex) = (n, ex)
        notFound = map snd $ filter ((== Nothing) . fst) (zip maybeFs fieldNames)

deleteTreeNodes :: WorkflowCommand
deleteTreeNodes rule ctx = if Nothing `elem` maybeNodes
    then err ("Nodes for delete not found: " ++ show notFound) ctx
    else ok $ ctx {currentTree = foldl removeNode (currentTree ctx) (catMaybes maybeNodes)}
    where
        removeNode :: SyntacticTree -> Constant -> SyntacticTree
        removeNode t (CTreePart n _) = findAndRemoveNode n t
        removeNode t _ = t
        maybeNodes = map (`M.lookup` variableStates ctx) (toDelete $ deletedNodes rule)
        notFound = map snd $ filter ((== Nothing) . fst) $ zip maybeNodes (toDelete $ deletedNodes rule)
        toDelete (DeleteNodes ns) = ns

checkStop :: WorkflowCommand
checkStop rule ctx = if stop rule then debug "Stop." ctx {state = Finished} else ok ctx

ok :: Context -> WorkFlowResult
ok ctx = Right [ctx]

type LogFn = LogString -> Context -> WorkFlowResult

err :: LogFn
err l ctx = Left [ctx {workingLog = addLogLine Error l (workingLog ctx)}]

warning :: LogFn
warning l ctx = ok ctx {workingLog = addLogLine Warning l (workingLog ctx)}

debug :: LogFn
debug l ctx = ok ctx {workingLog = addLogLine Debug l (workingLog ctx)}
