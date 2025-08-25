module Editor.Tests where

import Data.Map (fromList)
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

import CDDB.Types
import CDDB.SyntacticTree

testTree1 = Tag "S" []
testTree2 = Tag "S" [Tag "NP" [Word "DET" "the", Word "N" "cat"], Tag "VP" [Word "V" "chase", Tag "NP" [Word "DET" "a", Word "N" "mouse"]]]

testFilter1 = FilterTag Nothing "S" []
testFilter2 = FilterTag Nothing "S" [FilterTag (Just "np") "NP" [Asterisk], FilterTag (Just "vp") "VP" [FilterWord "V" "chase"], Asterisk]

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