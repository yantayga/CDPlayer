{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Templates where

import qualified Data.Map as M

import GHC.Read
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.Parser

type PrimitiveTemplates = M.Map Name FieldDefinitions

data PrimitiveTemplate = PrimitiveTemplate Name FieldDefinitions

type FieldDefinitions = [VariableName]

addTemplate :: PrimitiveTemplate -> PrimitiveTemplates -> PrimitiveTemplates
addTemplate (PrimitiveTemplate t fs) = M.insert t fs

deleteTemplates :: PrimitiveTemplates -> [Name] -> PrimitiveTemplates
deleteTemplates = foldl (flip M.delete)

instance Show PrimitiveTemplate where
    show :: PrimitiveTemplate -> String
    show (PrimitiveTemplate t fs) = t ++ ": " ++ unwords fs

instance Read PrimitiveTemplate where
    readPrec = do
        L.Ident s <- lexP
        fs <- spaceList
        return $ PrimitiveTemplate s fs

templateDesc :: PrimitiveTemplate -> String
templateDesc (PrimitiveTemplate t fs) = "Name: " ++ t ++ "\nFields: " ++ unwords fs ++ "\n"

unpackTemplates :: PrimitiveTemplates -> [PrimitiveTemplate]
unpackTemplates = map (uncurry PrimitiveTemplate) . M.toList

findTemplate :: PrimitiveTemplates -> Name -> Maybe PrimitiveTemplate
findTemplate ts name = M.lookup name ts >>= \t -> return $ PrimitiveTemplate name t
