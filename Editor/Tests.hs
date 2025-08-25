module Editor.Tests where

import CDDB.SyntacticTree

testSTree1 = Tag "S" []
testSTree2 = Tag "S" [Tag "NP" [Word "DET" "the", Word "N" "cat"], Tag "VP" [Word "V" "chase", Tag "NP" [Word "DET" "a", Word "N" "mouse"]]]

testFilter1 = FilterTag "S" []
testFilter2 = FilterTag "S" [FilterTag "NP" [], FilterTag "VP" [FilterWord "V" "chase"], Asterisk]