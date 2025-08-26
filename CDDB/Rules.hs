{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Rules where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe
import qualified Data.Map as M
import Data.UUID (UUID)

import CDDB.Types
import CDDB.Tree.Filter
import CDDB.Tree.Syntax

type Rules = M.Map RuleId Rule

data Rule = Rule Comment Score FilterExpression Locals Conditions Actions deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Score = Double
type RuleId = UUID

findRuleById :: Rules -> RuleId -> Maybe (RuleId, Rule)
findRuleById rules id = (id,) <$> M.lookup id rules

matchRuleAndFindPaths :: SyntacticTree -> Rule -> Maybe (Rule, VariablePaths)
matchRuleAndFindPaths t r@(Rule _ _ filterExpr _ _ _) = (r,) <$> matchFilterExpr t filterExpr

matchRulesAndFindPaths :: SyntacticTree -> Rules -> M.Map RuleId (Rule, VariablePaths)
matchRulesAndFindPaths t rs = M.mapMaybe (matchRuleAndFindPaths t) rs

ruleDesc :: (RuleId, Rule) -> String
ruleDesc (id, Rule comment score filterExpression locals conditions actions) =
    "Id: " ++ show id ++ "\n" ++
    "\tComment: " ++ show comment ++ "\n" ++
    "\tScore: " ++ show score ++ "\n" ++
    "\tFilterExpression: " ++ show filterExpression ++ "\n"

boundRuleDesc :: SyntacticTree -> (RuleId, (Rule, VariablePaths)) -> String
boundRuleDesc t (id, (r, vp)) = ruleDesc (id, r) ++ "\n\tVariable bounds:\n" ++ concat (M.mapWithKey (treePathDesc t) vp)

treePathDesc t vn path = "\t\t" ++ vn ++ " = " ++ show path ++ " -> " ++ (printTree $ findNode path t) ++ "\n"
    where
        printTree Nothing = "<not found>"
        printTree (Just t) = show t
