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

type FilterRules = Arguments -> [(RuleId, Rule)] -> Either String ([(RuleId, Rule)], [(RuleId, Rule)])

cmdShowRules :: Command
cmdShowRules [] state = do
    mapM_ printFn $ zipFrom 0 (currentRules state)
    return $ Right state
    where
        printFn (idx, r) = do
            print idx
            putStrLn $ ruleDesc r
cmdShowRules _ _ = errTooManyArguments

cmdAddRule :: Command
cmdAddRule [] state = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = (newUUID, newRule) : currentRules state}
cmdAddRule _ _ = errTooManyArguments

cmdDeleteRules :: FilterRules -> Command
cmdDeleteRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, _) -> return $ Right state {currentRules = passed}

cmdWipeRules :: FilterRules -> Command
cmdWipeRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, used) -> return $ Right state {cddb = deleteRulesToCDDB (cddb state) $ map fst used, currentRules = passed}

cmdWriteRules :: FilterRules -> Command
cmdWriteRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (_, used) -> return $ Right state {cddb = addRulesToCDDB (cddb state) used}

cmdRenewRules :: FilterRules -> Command
cmdRenewRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, used) -> do
        uuids <- replicateM (length used) nextUUID
        return $ Right state {currentRules = passed ++ zip (map fromJust uuids) (map snd used)}

filterByN :: FilterRules
filterByN [arg] ls = let v = readEither arg in
    case v of
        Left err -> Left err
        Right n -> if n < 0 || n >= length ls then
            Left "Index put of range"
            else Right (crsB ++ drop1 crsA, [head crsA])
            where
                (crsB, crsA) = splitAt n ls
filterByN [] _ = Left "Not enough arguments"
filterByN _ _ = Left "Too many arguments"

useAll :: FilterRules
useAll [] ls = Right (ls, [])
useAll _ _ = Left "Too many arguments"

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
