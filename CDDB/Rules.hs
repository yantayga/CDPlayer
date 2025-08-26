{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Rules where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

import CDDB.Types
import CDDB.Tree.Filter
import CDDB.Tree.Syntax

type Rules = [Rule]

data Rule = Rule Comment Score FilterExpression Locals Conditions Actions deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Score = Double

matchRuleAndFindPaths :: SyntacticTree -> Rule -> Maybe VariablePaths
matchRuleAndFindPaths t r@(Rule _ _ filterExpr _ _ _) = matchFilterExpr t filterExpr
