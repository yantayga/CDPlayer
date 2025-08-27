{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Templates where

import qualified Data.Map as M
import Data.List.Extra (zipFrom)

import CDDB.CDDB
import CDDB.Templates

import Editor.Command.Types
import Editor.Command.Common
import Editor.Command.Errors
import Editor.Command.Help

templateCommands :: CommandMap
templateCommands = M.fromList [
        ("help",   CommandDef (cmdHelp templateCommands) "This help."),
        ("add",    CommandDef cmdAddTemplate "Add template <name> <fields0> <fields1> ... <fieldsN> to CDDB."),
        ("show",   CommandDef cmdShowTemplates "Show all templates in CDDB"),
        ("delete", CommandDef cmdDeleteTemplate "Delete templates <name0> <name1> ... <nameN> from CDDB.")
    ]

cmdShowTemplates :: Command
cmdShowTemplates [] state = do
    mapM_ printFn $ zipFrom 0 (unpackTemplates $ templates $ cddb state)
    return $ Right state
    where
        printFn (idx, r) = do
            print idx
            putStrLn $ templateDesc r
cmdShowTemplates _ _ = return errTooManyArguments

cmdAddTemplate :: Command
cmdAddTemplate (n:fs) state = return $ Right state {cddb = addTemplatesToCDDB (cddb state) [PrimitiveTemplate n fs]}
cmdAddTemplate _ _ = return errNotEnoughArguments

cmdDeleteTemplate :: Command
cmdDeleteTemplate [] _ = return errNotEnoughArguments
cmdDeleteTemplate ns state = return $ Right state {cddb = deleteTemplatesFromCDDB (cddb state) ns}
