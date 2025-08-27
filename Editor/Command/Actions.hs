{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Actions where

import qualified Data.Map as M
import Text.Read (readEither)

import CDDB.Rules
import CDDB.Utils

import Editor.Command.Types
import Editor.Command.Help

actionCommands :: CommandMap
actionCommands = M.fromList [
        ("help",   CommandDef (cmdHelp actionCommands) "This help."),
        ("add",    CommandDef cmdAddAction "Add action at pos <n> to rule(s)."),
        ("delete", CommandDef cmdDeleteActio "Delete action <n> to rule(s)."),
        ("update", CommandDef cmdUpdateAction "Update action <n> to rule(s).")
    ]

cmdAddAction :: Command
cmdAddAction (arg:args) state = 
    case (readEither (unwords args), readEither arg) of
        (Right action, Right n) -> return $ Right state {currentRules = map (addAction n action) (currentRules state)}
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
    where
        addAction n a (ruleId, Rule comment score filterExpression locals conditions actions) = (ruleId, Rule comment score filterExpression locals conditions (insertInNthPosition n a actions))
cmdAddAction [] _ = return $ Left "Not enough arguments"

cmdDeleteActio :: Command
cmdDeleteActio [arg] state  = 
    case readEither arg of
        Right n -> return $ Right state {currentRules = map (deleteAction n) (currentRules state)}
        Left err -> return $ Left err
    where
        deleteAction n (ruleId, Rule comment score filterExpression locals conditions actions) = (ruleId, Rule comment score filterExpression locals conditions (deleteNthElement n actions))
cmdDeleteActio [] _ = return $ Left "Not enough arguments"
cmdDeleteActio _ _ = return $ Left "Too many arguments"

cmdUpdateAction :: Command
cmdUpdateAction (arg:args) state  =
    case (readEither (unwords args), readEither arg) of
        (Right action, Right n) -> return $ Right state {currentRules = map (replaceAction n action) (currentRules state)}
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
    where
        replaceAction n a (ruleId, Rule comment score filterExpression locals conditions actions) = (ruleId, Rule comment score filterExpression locals conditions (updateNthElement n a actions))
cmdUpdateAction [] _ = return $ Left "Not enough arguments"
