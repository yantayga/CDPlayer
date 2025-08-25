{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner where

import System.IO.Unsafe

import GHC.Generics
import Data.Aeson hiding (Null)
import qualified Data.Map as M
import Data.Maybe
import Data.List ((!!))
import Data.Either.Extra
import Control.Monad
import Control.Applicative

import CDDB.Types
import CDDB.Expressions
import CDDB.SyntacticTree

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
matchRules t cddb = catMaybes $ map (matchRule t) $ rules cddb

matchRule :: SyntacticTree -> Rule -> Maybe (Rule, VariableStates)
matchRule t r@(Rule _ _ filterExpr _ _ _) = (r,) <$> matchFilter t 0 filterExpr []

matchFilter :: SyntacticTree -> TreePos -> FilterExpression -> TreePath -> Maybe VariableStates
matchFilter _ _ Asterisk _ = Just emptyVariableStates
matchFilter t@(Tag id ts) pos (FilterTag Nothing fid fs) path = guard (id == fid) >> (matchFilters ts pos fs $ pos:path)
matchFilter t@(Tag id ts) pos (FilterTag (Just vn) fid fs) path = guard (id == fid) >> (matchFilters ts pos fs $ pos:path) >>= \vs -> Just $ M.insert vn (CTreePart $ tail $ reverse $ pos:path) vs
matchFilter (Word id s) pos (FilterWord Nothing fid fs) _ = guard (id == fid && s == fs) >> Just emptyVariableStates
matchFilter (Word id s) pos (FilterWord (Just vn) fid fs) path = guard (id == fid && s == fs) >> Just (M.singleton vn (CTreePart $ reverse $ pos:path))
matchFilter _ _ _ _ = Nothing

matchFilters :: [SyntacticTree] -> TreePos -> [FilterExpression] -> TreePath -> Maybe VariableStates
matchFilters [] _ [] _ = Just emptyVariableStates
matchFilters _ _ [] _ = Nothing
matchFilters [] pos (Asterisk: sfs) path = matchFilters [] pos sfs path
matchFilters [] _ _ _ = Nothing
matchFilters ts@(t: sts) pos fs@(Asterisk: sfs) path = matchFilters ts pos sfs path <|> matchFilters sts (pos + 1) fs path
matchFilters (t: sts) pos (f: sfs) path = do
    vs1 <- matchFilter t pos f path
    vs2 <- matchFilters sts (pos + 1) sfs path
    return $ M.unionWith const vs1 vs2

evaluateRule :: Context -> Rule -> Context
evaluateRule ctx rule@(Rule _ ruleScore _ locals conditions actions) =
    if applicable
        then doActions (ctx {variableStates = states', accumulatedScore = accumulatedScore ctx * ruleScore}) actions
        else ctx
    where
        states' = addLocals (variableStates ctx) locals
        applicable = checkConditions states' conditions

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

findAndRemoveNode :: TreePath -> SyntacticTree -> SyntacticTree
findAndRemoveNode (n:[]) (Tag id ts) = Tag id $ (take n ts) ++ (drop (n + 1) ts)
findAndRemoveNode (n:ns) (Tag id ts) = Tag id $ (take n ts) ++ findAndRemoveNode ns (ts !! n) : (drop (n + 1) ts)
findAndRemoveNode n t = t

addFact :: Context -> Primitive -> Context
addFact ctx p = ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) p: (accumulatedKnowledge ctx)}

addLocals :: VariableStates -> Locals -> VariableStates
addLocals states ls = foldl addVariableDef states ls

addVariableDef :: VariableStates -> VariableDef -> VariableStates
addVariableDef states (VariableDef name expr) = M.insert name value states
    where
        value = evaluateExpression states expr

checkConditions :: VariableStates -> Conditions -> Bool
checkConditions states exprs = all (== (CBoolean True)) $ map (evaluateExpression states) exprs

evaluateFact :: VariableStates -> Primitive -> Fact
evaluateFact states (Primitive name fieldVariables) = Fact name $ map (evaluateExpression states) fieldVariables



