{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner.Workflow (runWorkflow) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (foldM)

import CDDB.CDDB
import CDDB.Templates
import CDDB.Rules
import CDDB.AddFact
import CDDB.Logging
import CDDB.Expression.VariableDefs
import CDDB.Expression.Eval
import CDDB.Expression.Constants
import CDDB.Tree.Syntax
import CDDB.Runner.Context

type WorkflowCommand = Rule -> Context -> Either Context Context

ruleWorkflow :: [WorkflowCommand]
ruleWorkflow = [
    bindLocalVariables,
    checkRuleConditions,
    addFacts,
    deleteTreeNodes,
    checkStop
    ]

runWorkflow :: WorkflowCommand
runWorkflow rule ctx = foldM (flip ($)) ctx $ map ($ rule) ruleWorkflow

bindLocalVariables :: WorkflowCommand
bindLocalVariables rule ctx = Right ctx {variableStates = foldl addVariableDef (variableStates ctx) (locals rule)}

checkRuleConditions :: WorkflowCommand
checkRuleConditions rule ctx = if all ((== CBoolean True) . evaluateExpression (variableStates ctx)) (conditions rule)
    then Right $ debug "Rule passed conditions." ctx
    else Left $ debug "Rule has not passed conditions." ctx

addFacts :: WorkflowCommand
addFacts rule ctx = foldM addOneFact ctx (facts rule)

addOneFact :: Context -> AddFact -> Either Context Context
addOneFact ctx (AddFact name variableDefs) = case findTemplate (templates $ db ctx) name of
    Nothing -> Left $ warning ("Add fact failed: template " ++ name ++ "not found.") ctx
    Just template -> addOneFact' template variableDefs ctx

addOneFact' :: PrimitiveTemplate -> VariableDefs -> Context -> Either Context Context
addOneFact' (PrimitiveTemplate name fieldNames) variableDefs ctx =  if Nothing `elem` maybeFs
    then Left $ err ("Add fact failed: variables " ++ show notFound ++ " not found in template" ++ name ++ ".") ctx
    else Right $ ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) (catMaybes maybeFs): accumulatedKnowledge ctx}
    where
        evaluateFact states fieldVariables = Fact name $ map (evaluateExpression states) fieldVariables
        maybeFs = map (flip lookup $ map unpackDef variableDefs) fieldNames
        unpackDef (VariableDef n ex) = (n, ex)
        notFound = map snd $ filter ((== Nothing) . fst) (zip maybeFs fieldNames)

deleteTreeNodes :: WorkflowCommand
deleteTreeNodes rule ctx = if Nothing `elem` maybeNodes
    then Left $ err ("Nodes for delete not found: " ++ show notFound) ctx
    else Right $ ctx {currentTree = foldl removeNode (currentTree ctx) (catMaybes maybeNodes)}
    where
        removeNode :: SyntacticTree -> Constant -> SyntacticTree
        removeNode t (CTreePart n) = findAndRemoveNode n t
        removeNode t _ = t
        maybeNodes = map (`M.lookup` variableStates ctx) (deletedNodes rule)
        notFound = map snd $ filter ((== Nothing) . fst) $ zip maybeNodes (deletedNodes rule)

checkStop :: WorkflowCommand
checkStop rule ctx = Right $ if stop rule then debug "Stop." ctx {state = Finished} else ctx

type LogFn = LogString -> Context -> Context

err :: LogFn
err l ctx = ctx {workingLog = addLogLine Error l (workingLog ctx)}

warning :: LogFn
warning l ctx = ctx {workingLog = addLogLine Warning l (workingLog ctx)}

debug :: LogFn
debug l ctx = ctx {workingLog = addLogLine Debug l (workingLog ctx)}
