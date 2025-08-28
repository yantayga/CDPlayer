{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Common where

import qualified Data.Map as M
import Data.List (isInfixOf)
import Text.Read (readEither)

import CDDB.CDDB
import CDDB.Rules

import Editor.Command.Types
import Editor.Command.Errors
import Editor.Command.Settings

type FilterRules = Arguments -> [(RuleId, Rule)] -> Either String ([(RuleId, Rule)], [(RuleId, Rule)], [String])

runRuleCommand :: FilterRules -> CommandMap -> Command
runRuleCommand f cmds args state =  case f args (currentRules state) of
    Left err -> return $ Left err
    Right (passed, used, argsRest) -> do
        res <- runCommand cmds argsRest state {currentRules = used}
        case res of
            Left err -> return $ Left err
            Right newState -> return $ Right newState {currentRules = passed ++ currentRules newState}

runCommand :: CommandMap -> Command
runCommand cmds args state =
    case args of
        [] -> return errNotEnoughArguments
        (cmdName: restArgs) ->
            case M.lookup cmdName cmds of
                Nothing -> return $ Left $ "Command '" ++ cmdName ++ "' not found. Possible variants: " ++ findMostSimilar cmdName
                Just (CommandDef fn _) -> fn restArgs state
    where
        findMostSimilar cmdName = unwords $ (filter . isInfixOf) cmdName $ M.keys cmds

cmdGetField :: Show b => (ProgramState -> b) -> Command
cmdGetField accessor [] state = return $ Left $ show $ accessor state
cmdGetField _ _ _ = return errTooManyArguments

cmdSetField :: Read b => (ProgramState -> b -> ProgramState) -> Command
cmdSetField setter [val] state = let v = readEither val in
        case v of
                Left err -> return $ Left err
                Right a -> return $ Right $ setter state a
cmdSetField _ [] _ = return errNotEnoughArguments
cmdSetField _ _ _ = return errTooManyArguments

makeGetter :: (a -> b) -> (b -> c) -> (a -> c)
makeGetter = flip (.)

makeSetter :: (a -> b -> a) -> (b -> c -> b) -> (a -> b) -> (a -> c -> a)
makeSetter outer inner accessor a c = outer a $ inner (accessor a) c

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState {settings = settings, cddb = emptyCDDB, currentRules = [], isNotSaved = True, currentTemplate = Nothing}

