{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Rules where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.UUID (UUID)

import CDDB.Types
import CDDB.Actions
import CDDB.Tree.Filter
import CDDB.Tree.Syntax
import CDDB.Expression.Types
import CDDB.Expression.Expression

type Rules = M.Map RuleId Rule

type Locals = [VariableDef]

type Conditions = [Expression]

data Rule = Rule Comment Score FilterExpression Locals Conditions Actions deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Score = Double
type RuleId = UUID

newRule :: Rule
newRule = Rule "" 1.0 (Asterisk Nothing) [] [] []

addRules :: Rules -> [(RuleId, Rule)] -> Rules
addRules = foldl insertRule
    where
        insertRule rs (ruleId, rule) = M.insertWith const ruleId rule rs

deleteRules :: Rules -> [RuleId] -> Rules
deleteRules = foldl deleteRule
    where
        deleteRule rs ruleId = M.delete ruleId rs

findRuleById :: Rules -> RuleId -> Maybe (RuleId, Rule)
findRuleById rules ruleId = (ruleId,) <$> M.lookup ruleId rules

matchRuleAndFindPaths :: SyntacticTree -> Rule -> Maybe (Rule, VariablePaths)
matchRuleAndFindPaths t r@(Rule _ _ filterExpr _ _ _) = (r,) <$> matchFilterExpr t filterExpr

matchRulesAndFindPaths :: SyntacticTree -> Rules -> M.Map RuleId (Rule, VariablePaths)
matchRulesAndFindPaths t = M.mapMaybe (matchRuleAndFindPaths t)

ruleDesc :: (RuleId, Rule) -> String
ruleDesc (ruleId, Rule comment score filterExpression _ _ actions) =
    "Id: " ++ show ruleId ++ "\n" ++
    "\tComment: " ++ show comment ++ "\n" ++
    "\tScore: " ++ show score ++ "\n" ++
    "\tFilterExpression: " ++ show filterExpression ++ "\n" ++
    "\tActions: " ++ intercalate "\n\t\t" (map show actions) ++ "\n"

boundRuleDesc :: SyntacticTree -> (RuleId, (Rule, VariablePaths)) -> String
boundRuleDesc t (ruleId, (r, vp)) = ruleDesc (ruleId, r) ++ "\n\tVariable bounds:\n" ++ concat (M.mapWithKey (treePathDesc t) vp)

treePathDesc :: SyntacticTree -> String -> TreePath -> String
treePathDesc t vn path = "\t\t" ++ vn ++ " = " ++ show path ++ " -> " ++ printTree (findNode path t) ++ "\n"
    where
        printTree Nothing = "<not found>"
        printTree (Just st) = show st
