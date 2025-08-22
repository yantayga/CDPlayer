{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner where

import GHC.Generics
import Data.Aeson hiding (Null)
import qualified Data.Map as M
import Data.Maybe
import Data.Either.Extra
import Control.Monad

import CDDB.Types
import CDDB.Expressions


data Context = Context
        VariableStates  -- current variables
        Score           -- accumulated score
        Knowledge       -- knowledge we accumulated

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



