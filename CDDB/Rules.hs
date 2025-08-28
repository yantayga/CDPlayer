{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Rules where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.UUID (UUID)

import CDDB.Types
import CDDB.AddFact
import CDDB.Tree.Filter
import CDDB.Tree.Syntax
import CDDB.Expression.VariableDefs
import CDDB.Expression.Expression

type Rules = M.Map RuleId Rule

type Locals = VariableDefs

type Conditions = [Expression]

type AddFacts = [AddFact]


data Rule = Rule {
        comment :: Comment,
        score :: Score,
        filterExpression :: FilterExpression,
        locals :: Locals,
        conditions :: Conditions,
        parent :: SyntacticTree,
        facts :: AddFacts,
        deletedNodes :: [VariableName],
        stop :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Score = Double
type RuleId = UUID

newRule :: Rule
newRule = Rule "" 1.0 (Asterisk Nothing) [] [] (Tag "S" []) [] [] False

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
matchRuleAndFindPaths t rule = (rule,) <$> matchFilterExpr t (filterExpression rule)

matchRulesAndFindPaths :: SyntacticTree -> Rules -> M.Map RuleId (Rule, VariablePaths)
matchRulesAndFindPaths t = M.mapMaybe (matchRuleAndFindPaths t)

ruleDesc :: (RuleId, Rule) -> String
ruleDesc (ruleId, rule) =
    "Rule Id: " ++ show ruleId ++
    "\n\tComment: " ++ show (comment rule) ++
    "\n\tScore: " ++ show (score rule) ++
    "\n\tFilterExpression: " ++ show (filterExpression rule) ++
    "\n\tLocals: " ++ intercalate "\n\t\t" (map show $ locals rule) ++
    "\n\tConditions: " ++ intercalate "\n\t\t" (map show $ conditions rule) ++
    "\n\tFacts to add: " ++ intercalate "\n\t\t" (map show $ facts rule) ++
    "\n\tDelete nodes: " ++ intercalate "\n\t\t" (map show $ deletedNodes rule) ++
    "\n\tStop: " ++ show (stop rule) ++ "\n"

boundRuleDesc :: SyntacticTree -> (RuleId, (Rule, VariablePaths)) -> String
boundRuleDesc t (ruleId, (rule, vp)) = ruleDesc (ruleId, rule) ++ "\n\tVariable bounds:\n" ++ concat (M.mapWithKey (treePathDesc t) vp)

treePathDesc :: SyntacticTree -> String -> TreePath -> String
treePathDesc t vn path = "\t\t" ++ vn ++ " = " ++ show path ++ " -> " ++ printTree (findNode path t) ++ "\n"
    where
        printTree Nothing = "<not found>"
        printTree (Just st) = show st
