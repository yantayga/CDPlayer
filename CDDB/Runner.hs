{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner where

import System.IO.Unsafe

import GHC.Generics
import Data.Aeson hiding (Null)
import qualified Data.Map as M
import Data.Maybe (Maybe(..), catMaybes)

import Control.Monad
import Control.Applicative

import CDDB.Types
import CDDB.Rules
import CDDB.CDDB
import CDDB.Expression.Types
import CDDB.Expression.Eval
import CDDB.Tree.Syntax
import CDDB.Tree.Filter
import CDDB.Utils

data ContextState = Finished | NonFinished deriving (Eq, Ord)

data Context = Context {
        variableStates :: VariableStates,
        currentTree :: SyntacticTree,
        accumulatedScore :: Score,
        accumulatedKnowledge :: Knowledge,
        state :: ContextState
    }

applyTree :: CDDB -> SyntacticTree -> [Context]
applyTree db t = filter ((== Finished) . state) $ applyTreeWithContext db $ emptyContext t

applyTreeWithContext :: CDDB -> Context -> [Context]
applyTreeWithContext db ctx = finished ++ concatMap subApply nonfinished
    where
        (finished, nonfinished) = span ((== Finished) . state) $ applyTreeOnce db ctx
        subApply ctx = applyTreeWithContext db $ resetContext ctx

applyTreeOnce :: CDDB -> Context-> [Context]
applyTreeOnce db ctx = map evaluateRuleAt $ matchRules (currentTree ctx) db
    where
        evaluateRuleAt (r, vs) = evaluateRule ctx {variableStates = vs} r

resetContext :: Context -> Context
resetContext ctx = ctx {variableStates = emptyVariableStates, state = NonFinished}

emptyContext :: SyntacticTree -> Context
emptyContext t = Context {
        variableStates = emptyVariableStates,
        currentTree = t,
        accumulatedScore = 1.0,
        accumulatedKnowledge = [],
        state = NonFinished
    }

matchRules :: SyntacticTree -> CDDB -> [(Rule, VariableStates)]
matchRules t cddb = map (mapSnd (M.map CTreePart)) $ M.elems $ matchRulesAndFindPaths t $ rules cddb

evaluateRule :: Context -> Rule -> Context
evaluateRule ctx rule@(Rule _ ruleScore _ locals conditions actions) =
    if applicable
        then doActions (ctx {variableStates = states, accumulatedScore = accumulatedScore ctx * ruleScore}) actions
        else ctx
    where
        states   = addLocals (variableStates ctx) locals
        applicable = checkConditions states conditions

doActions :: Context -> Actions -> Context
doActions ctx actions = foldl doAction ctx actions

doAction :: Context -> Action -> Context
doAction ctx Stop = ctx {state = Finished}
doAction ctx (AddFact p) = addFact ctx p
doAction ctx (Delete as) = ctx {currentTree = removeNodes (currentTree ctx) $ map ((M.!) (variableStates ctx)) as}

removeNodes :: SyntacticTree -> [Constant] -> SyntacticTree
removeNodes t vs = foldl removeNode t vs

removeNode :: SyntacticTree -> Constant -> SyntacticTree
removeNode t (CTreePart n) = findAndRemoveNode n t
removeNode t _ = t

addFact :: Context -> Primitive -> Context
addFact ctx p = ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) p: (accumulatedKnowledge ctx)}

addLocals :: VariableStates -> Locals -> VariableStates
addLocals states ls = foldl addVariableDef states ls

checkConditions :: VariableStates -> Conditions -> Bool
checkConditions states exprs = all (== (CBoolean True)) $ map (evaluateExpression states) exprs

evaluateFact :: VariableStates -> Primitive -> Fact
evaluateFact states (Primitive name fieldVariables) = Fact name $ map (evaluateExpression states) fieldVariables



