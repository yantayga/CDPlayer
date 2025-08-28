{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner.Runner where

import CDDB.CDDB
import CDDB.Runner.Context
import CDDB.Runner.Workflow
import CDDB.Tree.Syntax

applyTree :: CDDB -> SyntacticTree -> RecursionDepth -> [Context]
applyTree db t mrd = applyTreeWithContext (emptyContext db t mrd)
