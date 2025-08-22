{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner where

import System.IO.Unsafe

import GHC.Generics
import Data.Aeson hiding (Null)
import qualified Data.Map as M
import Data.Maybe
import Data.Either.Extra
import Control.Monad

import CDDB.Types
import CDDB.Expressions
import CDDB.SyntacticTree

data Context = Context
        VariableStates  -- current variables
        Score           -- accumulated score
        Knowledge       -- knowledge we accumulated

applyTree :: SyntacticTree -> CDDB -> Either Context Context
applyTree t db = foldM evaluateRule emptyContext rules
    where
        rules = matchRules t db

emptyContext :: Context
emptyContext = Context (VariableStates M.empty) 1.0 (Knowledge [])

matchRules :: SyntacticTree -> CDDB -> [Rule]
matchRules t (CDDB _ _ _ _ (Rules rules) _) = filter (matchRule t) rules

matchRule :: SyntacticTree -> Rule -> Bool
matchRule t (Rule _ _ filter _ _ _) = matchFilter t filter

matchFilter :: SyntacticTree -> FilterExpression -> Bool
matchFilter _ Asterix = True
matchFilter (Tag id ts) (FilterTag fid fs) = id == fid && matchFilter' ts fs
matchFilter (Word s) (FilterWord fs) = s == fs
matchFilter _ _ = False

matchFilter' :: [SyntacticTree] -> [FilterExpression] -> Bool
matchFilter' [] [] = True
matchFilter' _ [] = False
matchFilter' [] (Asterix: sfs) = matchFilter' [] sfs
matchFilter' [] _ = False
matchFilter' ts@(t: sts) fs@(Asterix: sfs) = matchFilter' sts fs  -- maitched t with *, not consumed t, consumed *
                                          || matchFilter' ts sfs  -- maitched [] with *, consumed *
                                          -- || matchFilter' sts sfs -- maitched t with *, consumed t, not consumed *
matchFilter' (t: sts) (f: sfs) = matchFilter t f &&  matchFilter' sts sfs

evaluateRule :: Context -> Rule -> Either Context Context
evaluateRule ctx@(Context states score kn) rule@(Rule _ ruleScore _ locals conditions actions) =
    if applicable
        then doActions (Context states' (score*ruleScore) kn) actions
        else Right ctx
    where
        states' = addLocals states locals
        applicable = checkConditions states' conditions

doActions :: Context -> Actions -> Either Context Context
doActions ctx (Actions actions) = foldM doAction ctx actions

doAction :: Context -> Action -> Either Context Context
doAction ctx Stop = Left ctx
doAction ctx (AddFact p) = Right $ addFact ctx p
doAction ctx (Delete a) = undefined

addFact :: Context -> Primitive -> Context
addFact (Context states score (Knowledge kn)) p = Context states score (Knowledge $ evaluateFact states p: kn)

addLocals :: VariableStates -> Locals -> VariableStates
addLocals states (Locals ls) = foldl addVariableDef states ls

addVariableDef :: VariableStates -> VariableDef -> VariableStates
addVariableDef states@(VariableStates map) (VariableDef name expr) = VariableStates $ M.insert name value map
    where
        value = evaluateExpression states expr

checkConditions :: VariableStates -> Conditions -> Bool
checkConditions states (Conditions exprs) = all (== (CBoolean True)) $ map (evaluateExpression states) exprs

evaluateFact :: VariableStates -> Primitive -> Fact
evaluateFact states (Primitive name (FieldVariables fieldVariables)) = Fact name $ FieldConstants $ map (evaluateExpression states) fieldVariables



