{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Rules where

import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromJust)
import Data.UUID (fromString)
import Data.UUID.V1 (nextUUID)
import Text.Read
import Data.List.Extra (zipFrom, drop1)

import Control.Monad (replicateM)

import CDDB.Rules
import CDDB.CDDB
import CDDB.Utils

import Editor.Command.Types
import Editor.Command.Common

cmdShowRules :: Command
cmdShowRules [] state = do
    mapM_ printFn $ zipFrom 0 (currentRules state)
    return $ Right state
    where
        printFn (idx, r) = do
            print idx
            putStrLn $ ruleDesc r
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

cmdWriteRule :: Command
cmdWriteRule [arg] state = let v = readEither arg in
    case v of
        Left err -> return $ Left err
        Right ruleN -> if ruleN < 0 || ruleN >= length crs
            then return $ Left "Index out of range"
            else return $ Right state {cddb = addRulesToCDDB (cddb state) [crs !! ruleN]}
    where
        crs = currentRules state
cmdWriteRule [] _ = return $ Left "Not enough arguments"
cmdWriteRule _ _ = errTooManyArguments

cmdRenewRules :: Command
cmdRenewRules [] state = do
    uuids <- replicateM (length crs) nextUUID
    return $ Right state {currentRules = zip (map fromJust uuids) crs}
    where
        crs = map snd $ currentRules state
cmdRenewRules _ _ = errTooManyArguments

cmdRenewRule :: Command
cmdRenewRule [arg] state = let v = readEither arg in
    case v of
        Left err -> return $ Left err
        Right ruleN -> if ruleN < 0 || ruleN >= length (currentRules state)
            then return $ Left "Index out of range"
            else do
                Just uuid <- nextUUID
                return $ Right state {currentRules = crsB ++ [(uuid, snd $ head crsA)] ++ drop1 crsA}
            where
                (crsB, crsA) = splitAt ruleN $ currentRules state
cmdRenewRule [] _ = return $ Left "Not enough arguments"
cmdRenewRule _ _ = errTooManyArguments

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
