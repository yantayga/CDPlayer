{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Help where

import qualified Data.Map as M

import Editor.Command.Types

cmdHelp :: CommandMap -> Command
cmdHelp cmds args _ = return $ Left $ M.foldrWithKey (addCommandHelp args) "Commands:\nType <command> help for command help\n" cmds
    where
        addCommandHelp names key (CommandDef _ def) acc = if null names || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""
