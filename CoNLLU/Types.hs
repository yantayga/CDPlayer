module CoNLLU.Types where

import qualified Data.Map as M

type Word2Index = M.Map String WordIndex
type WordIndex = Int

data CoNLLUData = CoNLLUData {
    fileName :: String,
    sentences :: [CoNLLUSentense],
    fullWords :: Word2Index,
    initialWords :: Word2Index,
    uPOSTags :: Word2Index,
    xPOSTags :: Word2Index,
    featureNames :: Word2Index,
    featureValues :: Word2Index,
    depNames :: Word2Index,
    depRelNames :: Word2Index,
    logs :: String
    } deriving (Show)

data CoNLLUSentense = CoNLLUSentense {
        text :: String,
        items :: [CoNLLUWord]
    } deriving (Show)

type UPOSTagIndex = WordIndex -- https://universaldependencies.org/u/pos/index.html
type XPOSTagIndex = WordIndex

type Features = [(FeatureIndex, FeatureValue)] -- https://universaldependencies.org/u/feat/index.html
type FeatureIndex = WordIndex
type FeatureValue = WordIndex

type DepRelIndex = WordIndex -- https://universaldependencies.org/u/dep/index.html

data CoNLLUWord = CoNLLUWord {
        wordId :: (WordIndex, WordIndex),
        word :: WordIndex,
        initialWord :: WordIndex,
        uposTag :: UPOSTagIndex,
        xposTag :: XPOSTagIndex,
        features :: Features,
        depHead :: WordIndex,
        depRel :: DepRelIndex,
        misc :: String
    } deriving (Show)
