{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Runner  where

import qualified Data.Map as M

import CDDB.Runner

import Editor.Command.Types
import Editor.Command.Settings

cmdRun :: Command
cmdRun args state = if null results
    then return $ Left "Nothing happened..."
    else do
        mapM_ printResult results
        return $ Right state
    where
        tree = read $ unwords args
        results = applyTree (cddb state) tree (maxRecursionDepth $ settings state)
        printResult result = do
            putStr "Score:"
            print (accumulatedScore result)
            putStrLn "Tree after run:"
            print (currentTree result)
            putStrLn "Maximum recursion depth:"
            print (recursionDepth result)
            putStrLn "Accumulated knowledge:"
            print (accumulatedKnowledge result)
            return ()
