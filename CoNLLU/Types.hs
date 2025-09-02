{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module CoNLLU.Types where

import GHC.Generics
import qualified Data.Map.Strict as M
import qualified Data.Vector.Strict as V

import Data.Text

import Control.DeepSeq

type Word2Index = M.Map Text WordIndex
type WordIndex = Int

data CoNLLUData = CoNLLUData {
    fileName :: Text,
    sentences :: V.Vector CoNLLUSentense,
    fullWords :: Word2Index,
    initialWords :: Word2Index,
    uPOSTags :: Word2Index,
    xPOSTags :: Word2Index,
    featureNames :: Word2Index,
    featureValues :: Word2Index,
    depNames :: Word2Index,
    depRelNames :: Word2Index,
    startWord :: WordIndex,
    endWord :: WordIndex
    } deriving (Show, Generic, NFData)

data CoNLLUSentense = CoNLLUSentense {
        text :: Text,
        items :: V.Vector CoNLLUWord
    } deriving (Show, Generic, NFData)

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
        misc :: Text
    } deriving (Show, Generic, NFData)
