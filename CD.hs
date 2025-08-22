{-# LANGUAGE OverloadedStrings #-}

module CD where

import CDDB.Types
import CDDB.Runner

import Data.Aeson
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
            (Contitions [])
            (Primitives [])
            (Actions [Stop])
        ,
        Rule "rule 2 comment" 0.9
            "S (NP VP)"
            (Locals [
                VariableDef "object" True "NP"
            ])
            (Contitions [
                UnOp IsNotNull (Variable "object")
            ])
            (Primitives [Primitive "MOVE" (FieldValues ["NP", "object", "", "", ""])])
            (Actions [])
    ]

testKnowledge = Knowledge []

testCDDB = CDDB "CDDB example" 1 Nothing testPrimitiveTemplates testRules testKnowledge

printCDDB cddb = do
    Data.ByteString.Lazy.putStr $ encode (toJSON cddb)
    Data.ByteString.Lazy.putStr $ "\n"