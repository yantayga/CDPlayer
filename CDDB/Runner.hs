{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner where

import qualified Data.Map as M

import CDDB.Types
import CDDB.Rules
import CDDB.CDDB
import CDDB.Expression.Types
import CDDB.Expression.Eval
import CDDB.Tree.Syntax
import CDDB.Utils

data ContextState = Finished | NonFinished deriving (Eq, Ord)

type RecursionDepth = Int

data Context = Context {
        variableStates :: VariableStates,
        currentTree :: SyntacticTree,
        accumulatedScore :: Score,
        accumulatedKnowledge :: Knowledge,
        state :: ContextState,
        recursionDepth :: RecursionDepth
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
        evaluateRuleAt (r, vs) = evaluateRule ctx {variableStates = vs} r

resetContext :: Context -> Context
resetContext ctx = ctx {variableStates = emptyVariableStates, state = NonFinished, recursionDepth = 1 + recursionDepth ctx}

emptyContext :: SyntacticTree -> Context
emptyContext t = Context {
        variableStates = emptyVariableStates,
        currentTree = t,
        accumulatedScore = 1.0,
        accumulatedKnowledge = [],
        state = NonFinished,
        recursionDepth = 0
    }

matchRules :: SyntacticTree -> CDDB -> [(Rule, VariableStates)]
matchRules t cddb = map (mapSnd (M.map CTreePart)) $ M.elems $ matchRulesAndFindPaths t $ rules cddb

evaluateRule :: Context -> Rule -> Context
evaluateRule ctx (Rule _ ruleScore _ locals conditions actions) =
    if applicable
        then doActions (ctx {variableStates = states, accumulatedScore = accumulatedScore ctx * ruleScore}) actions
        else ctx
    where
        states   = addLocals (variableStates ctx) locals
        applicable = checkConditions states conditions

doActions :: Context -> Actions -> Context
doActions = foldl doAction

doAction :: Context -> Action -> Context
doAction ctx Stop = ctx {state = Finished}
doAction ctx (AddFact p) = addFact ctx p
doAction ctx (Delete as) = ctx {currentTree = removeNodes (currentTree ctx) $ map (variableStates ctx M.!) as} -- TODO: M.! can fail! use mapMaybe

removeNodes :: SyntacticTree -> [Constant] -> SyntacticTree
removeNodes = foldl removeNode

removeNode :: SyntacticTree -> Constant -> SyntacticTree
removeNode t (CTreePart n) = findAndRemoveNode n t
removeNode t _ = t

addFact :: Context -> Primitive -> Context
addFact ctx p = ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) p: accumulatedKnowledge ctx}

addLocals :: VariableStates -> Locals -> VariableStates
addLocals = foldl addVariableDef

checkConditions :: VariableStates -> Conditions -> Bool
checkConditions states = all ((== CBoolean True) . evaluateExpression states)

evaluateFact :: VariableStates -> Primitive -> Fact
evaluateFact states (Primitive name fieldVariables) = Fact name $ map (evaluateExpression states) fieldVariables



