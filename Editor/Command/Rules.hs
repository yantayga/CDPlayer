{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Rules where

import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromJust)
import Data.UUID (fromString)
import Data.UUID.V1 (nextUUID)

import Control.Monad (replicateM)

import CDDB.Rules
import CDDB.CDDB
import CDDB.Utils

import Editor.Command.Types
import Editor.Command.Common

cmdShowRules :: Command
cmdShowRules [] state = do
    mapM_ (putStrLn . ruleDesc) (currentRules state)
    return $ Right state
cmdShowRules _ _ = errTooManyArguments

cmdNewRule :: Command
cmdNewRule [] state = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = [(newUUID, newRule)]}
cmdNewRule _ _ = errTooManyArguments

cmdClearRules :: Command
cmdClearRules [] state = return $ Right state {currentRules = []}
cmdClearRules _ _ = errTooManyArguments

cmdAddRule :: Command
cmdAddRule [] state = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = (newUUID, newRule) : currentRules state}
cmdAddRule _ _ = errTooManyArguments

cmdDeleteRule :: Command
cmdDeleteRule args state = return $ Right state {currentRules = filter isIdInIds $ currentRules state}
    where
        isIdInIds (ruleId, _) = elem ruleId $ mapMaybe fromString args

cmdWipeRule :: Command
cmdWipeRule args state = return $ Right state {cddb = deleteRulesToCDDB (cddb state) ids}
    where
        ids = mapMaybe fromString args

cmdWriteRules :: Command
cmdWriteRules [] state = return $ Right state {cddb = addRulesToCDDB (cddb state) (currentRules state)}
cmdWriteRules _ _ = errTooManyArguments

cmdRenewRules :: Command
cmdRenewRules [] state = do
    uuids <- replicateM (length crs) nextUUID
    return $ Right state {currentRules = zip (map fromJust uuids) crs}
    where
        crs = map snd $ currentRules state
cmdRenewRules _ _ = errTooManyArguments

cmdFilterRules :: Command
cmdFilterRules args state = printAndUpdateCurrentRules (boundRuleDesc tree) (mapSnd fst) rulesFound state
    where
        tree = read $ unwords args
        rulesFound = M.toList $ matchRulesAndFindPaths tree (rules $ cddb state)

cmdFindRules :: Command
cmdFindRules args state = printAndUpdateCurrentRules ruleDesc id rulesFound state
    where
        ids = mapMaybe fromString args
        rulesFound = mapMaybe (findCDDBRuleById $ cddb state) ids

printAndUpdateCurrentRules :: (a -> String) -> (a -> (RuleId, Rule)) -> [a] -> ProgramState -> IO (Either String ProgramState)
printAndUpdateCurrentRules pf pre rulesFound state =  if null rulesFound
    then return $ Left "No rules found"
    else do
        mapM_ (putStrLn . pf) rulesFound
        return $ Right state {currentRules = map pre rulesFound}
