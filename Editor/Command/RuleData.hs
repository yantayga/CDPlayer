{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.RuleData where

import qualified Data.Map as M
import Text.Read (readEither)

import CDDB.Rules
import CDDB.Utils

import Editor.Command.Types
import Editor.Command.Errors
import Editor.Command.Help

factsCommand :: CommandMap
factsCommand = M.fromList [
        ("help",   CommandDef (cmdHelp factsCommand) "This help."),
        ("add",    CommandDef (cmdAddItem updateFacts) "Add fact at pos <n> in rule(s)."),
        ("delete", CommandDef (cmdDeleteItem updateFacts) "Delete fact <n> in rule(s)."),
        ("update", CommandDef (cmdUpdateItem updateFacts) "Update fact <n> in rule(s).")
    ]

localsCommand :: CommandMap
localsCommand = M.fromList [
        ("help",   CommandDef (cmdHelp localsCommand) "This help."),
        ("add",    CommandDef (cmdAddItem updateLocals) "Add local variable at pos <n> in rule(s)."),
        ("delete", CommandDef (cmdDeleteItem updateLocals) "Delete local variable <n> in rule(s)."),
        ("update", CommandDef (cmdUpdateItem updateLocals) "Update local variable <n> in rule(s).")
    ]

conditionsCommand :: CommandMap
conditionsCommand = M.fromList [
        ("help",   CommandDef (cmdHelp conditionsCommand) "This help."),
        ("add",    CommandDef (cmdAddItem updateConditions) "Add condition at pos <n> in rule(s)."),
        ("delete", CommandDef (cmdDeleteItem updateConditions) "Delete condition <n> in rule(s)."),
        ("update", CommandDef (cmdUpdateItem updateConditions) "Update condition <n> in rule(s).")
    ]

updateFacts :: (AddFacts -> AddFacts) -> Rule -> Rule
updateFacts f rule = rule {facts = f (facts rule)}

updateLocals :: (Locals -> Locals) -> Rule -> Rule
updateLocals f rule = rule {locals = f (locals rule)}

updateConditions :: (Conditions -> Conditions) -> Rule -> Rule
updateConditions f rule = rule {conditions = f (conditions rule)}

cmdAddItem :: Read a => (([a] -> [a]) -> Rule -> Rule) -> Command
cmdAddItem updater (arg:args) state =
    case (readEither (unwords args), readEither arg) of
        (Right action, Right n) -> return $ Right state {currentRules = map (addItem n action) (currentRules state)}
        (_, Left err) -> return $ Left $ "1#" ++ arg ++ " " ++ (unwords args) ++ ": " ++  err
        (Left err, _) -> return $ Left $ "2#" ++ arg ++ " " ++ (unwords args) ++ ": " ++  err
    where
        addItem n a (ruleId, rule )= (ruleId, updater (insertInNthPosition n a) rule)
cmdAddItem _ [] _ = return errNotEnoughArguments

cmdDeleteItem :: Read a => (([a] -> [a]) -> Rule -> Rule) -> Command
cmdDeleteItem updater [arg] state  =
    case readEither arg of
        Right n -> return $ Right state {currentRules = map (deleteItem n) (currentRules state)}
        Left err -> return $ Left $ "#" ++ arg ++ ": " ++  err
    where
        deleteItem n (ruleId, rule) = (ruleId, updater (deleteNthElement n) rule)
cmdDeleteItem _ [] _ = return errNotEnoughArguments
cmdDeleteItem _ _ _ = return errTooManyArguments

cmdUpdateItem :: Read a => (([a] -> [a]) -> Rule -> Rule) -> Command
cmdUpdateItem updater (arg:args) state  =
    case (readEither (unwords args), readEither arg) of
        (Right action, Right n) -> return $ Right state {currentRules = map (replaceItem n action) (currentRules state)}
        (Left err, _) -> return $ Left $ "1#" ++ arg ++ " " ++ (unwords args) ++ ": " ++  err
        (_, Left err) -> return $ Left $ "2#" ++ arg ++ " " ++ (unwords args) ++ ": " ++  err
    where
        replaceItem n a (ruleId, rule) = (ruleId, updater (updateNthElement n a) rule)
cmdUpdateItem _ [] _ = return errNotEnoughArguments
