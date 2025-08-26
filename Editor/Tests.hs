module Editor.Tests where

import Data.Map (fromList)
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.UUID (fromWords64)

import CDDB.Types
import CDDB.Rules
import CDDB.CDDB
import CDDB.Tree.Syntax
import CDDB.Tree.Filter

testTree1 = Tag "S" []
testTree2 = Tag "S" [Tag "NP" [Word "DET" "the", Word "N" "cat"], Tag "VP" [Word "V" "chase", Tag "NP" [Word "DET" "a", Word "N" "mouse"]]]

testFilter1 = FilterTag (Just "s") "S" []
testFilter2 = FilterTag (Just "s") "S" [Asterisk, FilterTag (Just "np") "NP" [Asterisk], Asterisk]

testCDDB :: CDDB
testCDDB = CDDB {
        name = "Test CDDB",
        comment = "Test CDDB, just for debug",
        version = 1337,
        date = UTCTime (fromOrdinalDate 0 0) 0,
        templates = fromList [],
        rules = fromList [
            (fromWords64 6128981282234515924 12039885860129472512, Rule "Test rule" 0.1 testFilter2 [] [] [])
        ],
        kn = []
    }