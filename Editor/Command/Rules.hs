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
import Editor.Command.Errors
import Editor.Command.Help
import Editor.Command.RuleData

ruleCommands :: CommandMap
ruleCommands = M.fromList [
        ("help",       CommandDef (cmdHelp ruleCommands) "This help."),
        ("add",        CommandDef cmdAddRule "Add rule to current rules."),
        ("write",      CommandDef (cmdWriteRules filterByN) "Add/update rule #arg to cddb."),
        ("renew",      CommandDef (cmdRenewRules filterByN) "Regenerate rule #arg ids."),
        ("clear",      CommandDef (cmdDeleteRules filterByN) "Delete rule #arg from the current rules."),
        ("locals",     CommandDef (runRuleCommand filterByN localsCommand) "Manipulate locals of rule #arg."),
        ("conditions", CommandDef (runRuleCommand filterByN conditionsCommand) "Manipulate conditions of rule #arg."),
        ("facts",      CommandDef (runRuleCommand filterByN factsCommand) "Manipulate facts of rule #arg."),
        ("delNode",    CommandDef (cmdSetDeleteNodes filterByN) "Set list of nodes to delete for rule #arg."),
        ("stop",       CommandDef (cmdSetStop filterByN) "Set stop flag for rule #arg."),
        ("wipe",       CommandDef (cmdWipeRules filterByN) "Delete rule #arg with id from CDDB.")
    ]

rulesCommands :: CommandMap
rulesCommands = M.fromList [
        ("help",       CommandDef (cmdHelp rulesCommands) "This help."),
        ("show",       CommandDef cmdShowRules "Show current rules."),
        ("write",      CommandDef (cmdWriteRules useAll)"Add/update current rules to cddb."),
        ("renew",      CommandDef (cmdRenewRules useAll) "Regenerate current rules ids."),
        ("clear",      CommandDef (cmdDeleteRules useAll) "Clear current rules."),
        ("locals",     CommandDef (runRuleCommand useAll localsCommand) "Manipulate locals."),
        ("conditions", CommandDef (runRuleCommand useAll conditionsCommand) "Manipulate conditions."),
        ("facts",      CommandDef (runRuleCommand useAll factsCommand) "Manipulate facts."),
        ("delNode",    CommandDef (cmdSetDeleteNodes useAll) "Set list of nodes to delete for current rules."),
        ("stop",       CommandDef (cmdSetStop useAll) "Set stop flag for current rules."),
        ("wipe",       CommandDef (cmdWipeRules useAll) "Delete current rules from CDDB."),
        ("find",       CommandDef cmdFindRules "Find rules by ids and set them current."),
        ("filter",     CommandDef cmdFilterRules "Find rules by syntactic tree and set them current.")
    ]

cmdSetStop :: FilterRules -> Command
cmdSetStop f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, used, [flagArg]) -> case readEither flagArg of
        Left err -> return $ Left err
        Right flag -> return $ Right state {currentRules = passed ++ map (mapSnd (\r -> r {stop = flag})) used}
    Right (_, _, []) -> return errNotEnoughArguments
    Right (_, _, _) -> return errTooManyArguments

cmdSetDeleteNodes :: FilterRules -> Command
cmdSetDeleteNodes f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (_, _, []) -> return errNotEnoughArguments
    Right (passed, used, restArgs) -> case readEither (unwords restArgs) of
        Left err -> return $ Left $ unwords restArgs ++ ": " ++ err
        Right list -> return $ Right state {currentRules = passed ++ map (mapSnd (\r -> r {deletedNodes = list})) used}

cmdShowRules :: Command
cmdShowRules [] state = do
    mapM_ printFn $ zipFrom 0 (currentRules state)
    return $ Right state
    where
        printFn (idx, r) = do
            print idx
            putStrLn $ ruleDesc r
cmdShowRules _ _ = return errTooManyArguments

cmdAddRule :: Command
cmdAddRule [] state = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = (newUUID, newRule) : currentRules state}
cmdAddRule _ _ = return errTooManyArguments

cmdDeleteRules :: FilterRules -> Command
cmdDeleteRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, _, _) -> return $ Right state {currentRules = passed}

cmdWipeRules :: FilterRules -> Command
cmdWipeRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, used, _) -> return $ Right state {cddb = deleteRulesFromCDDB (cddb state) $ map fst used, currentRules = passed}

cmdWriteRules :: FilterRules -> Command
cmdWriteRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (_, used, _) -> return $ Right state {cddb = addRulesToCDDB (cddb state) used}

cmdRenewRules :: FilterRules -> Command
cmdRenewRules f args state = case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, used, _) -> do
        uuids <- replicateM (length used) nextUUID
        return $ Right state {currentRules = passed ++ zip (map fromJust uuids) (map snd used)}

filterByN :: FilterRules
filterByN (arg:argsRest) ls = case readEither arg of
    Left err -> Left err
    Right n -> if n < 0 || n >= length ls then errOutOfRange
        else Right (crsB ++ drop1 crsA, [head crsA], argsRest)
        where
            (crsB, crsA) = splitAt n ls
filterByN [] _ = errNotEnoughArguments

useAll :: FilterRules
useAll args ls = Right ([], ls, args)

cmdFilterRules :: Command
cmdFilterRules args state =  case readEither (unwords args) of
        Left err -> return $ Left err
        Right tree -> printAndUpdateCurrentRules boundRuleDesc (mapSnd fst) rulesFound state
            where
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
