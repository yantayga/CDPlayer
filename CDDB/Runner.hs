{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner where

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import CDDB.Types
import CDDB.CDDB
import CDDB.Rules
import CDDB.Actions
import CDDB.Templates
import CDDB.Expression.Eval
import CDDB.Expression.Constants
import CDDB.Expression.Expression
import CDDB.Expression.VariableDefs
import CDDB.Tree.Syntax
import CDDB.Logging
import CDDB.Utils

data ContextState = Finished | NonFinished deriving (Eq, Ord)

type RecursionDepth = Int

data Context = Context {
        variableStates :: VariableStates,
        currentTree :: SyntacticTree,
        accumulatedScore :: Score,
        accumulatedKnowledge :: Knowledge,
        state :: ContextState,
        recursionDepth :: RecursionDepth,
        workingLog :: Logs
    }

applyTree :: CDDB -> SyntacticTree -> RecursionDepth -> [Context]
applyTree db t = applyTreeWithContext db (emptyContext t)

applyTreeWithContext :: CDDB -> Context -> RecursionDepth -> [Context]
applyTreeWithContext db ctx mrd = finished ++ deep ++ concatMap subApply shallow
    where
        (finished, nonfinished) = span ((== Finished) . state) $ applyTreeOnce db ctx
        (shallow, deep) = span ((< mrd) . recursionDepth) nonfinished
        subApply sctx = applyTreeWithContext db (resetContext sctx) mrd

applyTreeOnce :: CDDB -> Context-> [Context]
applyTreeOnce db ctx = map evaluateRuleAt $ matchRules (currentTree ctx) db
    where
        evaluateRuleAt (r, vs) = evaluateRule (templates db) ctx {variableStates = vs} r

resetContext :: Context -> Context
resetContext ctx = ctx {variableStates = emptyVariableStates, state = NonFinished, recursionDepth = 1 + recursionDepth ctx}

emptyContext :: SyntacticTree -> Context
emptyContext t = Context {
        variableStates = emptyVariableStates,
        currentTree = t,
        accumulatedScore = 1.0,
        accumulatedKnowledge = [],
        state = NonFinished,
        recursionDepth = 0,
        workingLog = emptyLog
    }

matchRules :: SyntacticTree -> CDDB -> [(Rule, VariableStates)]
matchRules t cddb = map (mapSnd (M.map CTreePart)) $ M.elems $ matchRulesAndFindPaths t $ rules cddb

evaluateRule :: PrimitiveTemplates -> Context -> Rule -> Context
evaluateRule templates ctx (Rule _ ruleScore _ locals conditions actions) =
    if applicable
        then doActions templates (ctx {variableStates = states, accumulatedScore = accumulatedScore ctx * ruleScore}) actions
        else logged Debug "Rule has not passed conditions." ctx
    where
        states   = addLocals (variableStates ctx) locals
        applicable = checkConditions states conditions

doActions :: PrimitiveTemplates -> Context -> Actions -> Context
doActions templates = foldl (doAction templates)

doAction :: PrimitiveTemplates -> Context -> Action -> Context
doAction _ ctx Stop = logged Debug "Stop." ctx {state = Finished}
doAction templates ctx (AddFact name fieldVariables) = case findTemplate templates name of
        Nothing -> logged Warning ("Add fact failed: template " ++ name ++ "not found.") ctx
        Just template -> addFact ctx template fieldVariables
doAction _ ctx (Delete names) = ctx {currentTree = removeNodes (currentTree ctx) $ map (variableStates ctx M.!) names} -- TODO: M.! can fail! use mapMaybe

removeNodes :: SyntacticTree -> [Constant] -> SyntacticTree
removeNodes = foldl removeNode

removeNode :: SyntacticTree -> Constant -> SyntacticTree
removeNode t (CTreePart n) = findAndRemoveNode n t
removeNode t _ = t

addFact :: Context -> PrimitiveTemplate -> VariableDefs -> Context
addFact ctx (PrimitiveTemplate name fieldNames) fvs = if Nothing `elem` maybeFs
    then logged Warning ("Add fact failed: variables " ++ show notFound ++ " not found in template" ++ name ++ ".") ctx
    else ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) name (catMaybes maybeFs): accumulatedKnowledge ctx}
    where
         maybeFs = map (flip lookup $ map unpackDef fvs) fieldNames
         unpackDef (VariableDef n ex) = (n, ex)
         notFound = map snd $ filter ((== Nothing) . fst) $ zip maybeFs fieldNames

evaluateFact :: VariableStates -> Name -> [Expression] -> Fact
evaluateFact states name fieldVariables = Fact name $ map (evaluateExpression states) fieldVariables

addLocals :: VariableStates -> Locals -> VariableStates
addLocals = foldl addVariableDef

checkConditions :: VariableStates -> Conditions -> Bool
checkConditions states = all ((== CBoolean True) . evaluateExpression states)

logged :: LogLevel -> LogString -> Context -> Context
logged ll l ctx = ctx {workingLog = addLogLine ll l (workingLog ctx)}
