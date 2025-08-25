module Editor.Tests where

import Data.Map (fromList)
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

import CDDB.Types
import CDDB.SyntacticTree

testSTree1 = Tag "S" []
testSTree2 = Tag "S" [Tag "NP" [Word "DET" "the", Word "N" "cat"], Tag "VP" [Word "V" "chase", Tag "NP" [Word "DET" "a", Word "N" "mouse"]]]

testFilter1 = FilterTag "S" []
testFilter2 = FilterTag "S" [FilterTag "NP" [], FilterTag "VP" [FilterWord "V" "chase"], Asterisk]

testCDDB :: CDDB
testCDDB = CDDB {
        name = "Test CDDB",
        comment = "Test CDDB, just for debug",
        version = 1337,
        date = UTCTime (fromOrdinalDate 0 0) 0,
        templates = fromList [],
        rules = [
            Rule "Test rule" 0.1 testFilter2 [] [] []
        ],
        kn = []
    }