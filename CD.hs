{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CD where

import CDDB.Types
import CDDB.Runner

import Data.Aeson hiding (Null)
import Data.Map (fromList)
import Data.ByteString.Lazy

testPrimitiveTemplatesMap = fromList(
    [
        ("MOVE", (FieldDefinitions ["subject", "object", "source", "destination", "path"])),
        ("PROPERTY", (FieldDefinitions ["object", "name", "value"]))
    ]
    )

testPrimitiveTemplates = PrimitiveTemplates testPrimitiveTemplatesMap

testRules = Rules
    [
        Rule "rule 1 comment" 1.0
            "S ()"
            (Locals [])
            (Conditions [])
            (Actions [Stop])
        ,
        Rule "rule 2 comment" 0.9
            "S (NP VP)"
            (Locals [
                VariableDef "object" (Constant Null)
            ])
            (Conditions [
                UnOp IsNotNull (Variable "object")
            ])
            (Actions [
                AddFact (Primitive "MOVE" (FieldVariables [Variable "NP", Variable "object", Constant Null, Constant Null, Constant Null]))
            ])
    ]

testKnowledge = Knowledge []

testCDDB = CDDB "CDDB example" 1 Nothing testPrimitiveTemplates testRules testKnowledge

printCDDB cddb = do
    Data.ByteString.Lazy.putStr $ encode (toJSON cddb)
    Data.ByteString.Lazy.putStr $ "\n"