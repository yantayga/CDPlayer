{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner.Context where

import CDDB.Types
import CDDB.CDDB
import CDDB.Logging
import CDDB.Rules
import CDDB.Expression.Eval
import CDDB.Tree.Syntax

data ContextState = Finished | NonFinished deriving (Eq, Ord, Show)

type RecursionDepth = Int

data Context = Context {
        db :: CDDB,
        variableStates :: VariableStates,
        currentTree :: SyntacticTree,
        accumulatedScore :: Score,
        accumulatedKnowledge :: Knowledge,
        state :: ContextState,
        recursionDepth :: RecursionDepth,
        workingLog :: Logs
    }

resetContext :: Context -> Context
resetContext ctx = ctx {variableStates = emptyVariableStates, state = NonFinished, recursionDepth = 1 + recursionDepth ctx}

emptyContext :: CDDB -> SyntacticTree -> Context
emptyContext cddb t = Context {
        db = cddb,
        variableStates = emptyVariableStates,
        currentTree = t,
        accumulatedScore = 1.0,
        accumulatedKnowledge = [],
        state = NonFinished,
        recursionDepth = 0,
        workingLog = emptyLog
    }
