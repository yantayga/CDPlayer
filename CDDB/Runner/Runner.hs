{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner.Runner where

import qualified Data.Map as M
import Data.Either (rights)

import CDDB.CDDB
import CDDB.Rules
import CDDB.Expression.Eval
import CDDB.Expression.Constants
import CDDB.Runner.Context
import CDDB.Runner.Workflow
import CDDB.Tree.Syntax
import CDDB.Utils

applyTree :: CDDB -> SyntacticTree -> RecursionDepth -> [Context]
applyTree db t = applyTreeWithContext (emptyContext db t)

applyTreeWithContext :: Context -> RecursionDepth -> [Context]
applyTreeWithContext ctx mrd = finished ++ maxdepth ++ concatMap subApply shallow
    where
        (finished, nonfinished) = span ((== Finished) . state) $ applyTreeOnce ctx
        (shallow, maxdepth) = span ((< mrd) . recursionDepth) nonfinished
        subApply sctx = applyTreeWithContext (resetContext sctx) mrd

applyTreeOnce :: Context-> [Context]
applyTreeOnce ctx = rights $ map evaluateRuleAt $ matchRules (currentTree ctx) (rules (db ctx))
    where
        evaluateRuleAt (r, vs) = runWorkflow r $ ctx {variableStates = vs}

matchRules :: SyntacticTree -> Rules -> [(Rule, VariableStates)]
matchRules t rules = map (mapSnd (M.map CTreePart)) $ M.elems $ matchRulesAndFindPaths t rules
