-- Reading colluu data
-- https://universaldependencies.org/format.html
module POS.Conluu where

import qualified Data.Map as M
import Data.List (isPrefixOf, lookup, reverse)
import Data.List.Extra (notNull, split, dropPrefix, trim)
import Data.Maybe (catMaybes, fromMaybe)

import Control.Monad (foldM)

type Word2Index = M.Map String WordIndex
type WordIndex = Int

data ConluuData = ConluuData {
    fileName :: String,
    sentences :: [ConluuSentense],
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

data ConluuSentense = ConluuSentense {
        text :: String,
        items :: [ConluuWord]
    } deriving (Show)

type UPOSTagIndex = WordIndex -- https://universaldependencies.org/u/pos/index.html
type XPOSTagIndex = WordIndex

type Features = [(FeatureIndex, FeatureValue)] -- https://universaldependencies.org/u/feat/index.html
type FeatureIndex = WordIndex
type FeatureValue = WordIndex

type DepRelIndex = WordIndex -- https://universaldependencies.org/u/dep/index.html

data ConluuWord = ConluuWord {
        wordId :: (WordIndex, WordIndex),
        word :: WordIndex,
        initialWord :: WordIndex,
        oposTag :: UPOSTagIndex,
        xposTag :: XPOSTagIndex,
        features :: Features,
--        headWord :: Maybe WordIndex,
--        depRel :: DepRelIndex,
--        deps :: [(DepRelIndex, DepRelIndex)],
        misc :: String
    } deriving (Show)

parseConluu :: String -> Maybe ConluuData
parseConluu ss = parseConluuSentenses d $ lines ss
    where
        d = ConluuData {
                fileName = "test",
                sentences = [],
                fullWords = M.empty,
                initialWords = M.empty,
                uPOSTags = M.empty,
                xPOSTags = M.empty,
                featureNames = M.empty,
                featureValues = M.empty,
                depNames = M.empty,
                depRelNames = M.empty ,
                logs = ""
            }

parseConluuSentenses :: ConluuData -> [String] -> Maybe ConluuData
parseConluuSentenses d ss = foldM parseConluuSentense d $ filter notNull $ split null ss

parseConluuSentense :: ConluuData -> [String] -> Maybe ConluuData
parseConluuSentense d ss = case (lookup "text" params, parseWords d wordsLines) of
    (Just t, Just (d', is)) -> return d' {sentences = ConluuSentense {text = trim $ dropPrefix "=" t, items = reverse is} : sentences d'}
    (_, _) -> return d
    where
        (header, wordsLines) = span ("#" `isPrefixOf`) ss
        params = map (mapTuple2 trim . span (/= '=') . dropPrefix "#") header

mapTuple2 f (a,b) = (f a, f b)

parseWords :: ConluuData -> [String] -> Maybe (ConluuData, [ConluuWord])
parseWords d ss = foldM parseWord (d, []) ss

parseWord :: (ConluuData, [ConluuWord]) -> String -> Maybe (ConluuData, [ConluuWord])
parseWord (d, ws) s = do
    return (
        d {fullWords = fws, initialWords = iws, uPOSTags = upts, xPOSTags = xpts, featureNames = fnis, featureValues = fvis},
        (ConluuWord (read wid1, 0) wIdx iwIdx optIdx xptIdx ifs $ concat misc): ws
        )
    where
        (wid: w: iw: opt: xpt: fs:_: _: _: misc) = split (== '\t') s
        (wid1:wid2) = split (== '-') wid
        (wIdx, fws) = updateWord2IndexWith (fullWords d) w
        (iwIdx, iws) = updateWord2IndexWith (initialWords d) iw
        (optIdx, upts) = updateWord2IndexWith (uPOSTags d) opt
        (xptIdx, xpts)  = updateWord2IndexWith (xPOSTags d) xpt
        (ifs, fnis, fvis) = parseFeatures (featureNames d, featureValues d) fs

parseFeatures :: (Word2Index, Word2Index) -> String -> (Features, Word2Index, Word2Index)
parseFeatures (fnis, fvis) s = foldl update ([], fnis, fvis) pairs
    where
        pairs = map (span (/= '=')) $ split (== '|') s
        update :: (Features, Word2Index, Word2Index) -> (String, String) -> (Features, Word2Index, Word2Index)
        update (fs, fnis, fvis) (n, v) =
            let (ni, fnis') = updateWord2IndexWith fnis n
                (vi, fvis') = updateWord2IndexWith fvis $ dropPrefix "=" v
            in ((ni, vi):fs, fnis', fvis')

updateWord2IndexWith :: Word2Index -> String -> (WordIndex, Word2Index)
updateWord2IndexWith w2i s = case mix of
    Nothing -> (nextIx, M.insert s nextIx w2i)
    Just ix -> (ix, w2i)
    where
        mix = M.lookup s w2i
        nextIx = M.size w2i