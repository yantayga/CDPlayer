-- Reading colluu data
-- https://universaldependencies.org/format.html
module CoNLLU.Parse where

import qualified Data.Map as M
import Data.List ((!!), isPrefixOf, lookup, reverse)
import Data.List.Extra (notNull, split, dropPrefix, trim)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.Tuple (swap)

import Control.Monad (foldM)

import CoNLLU.Types

parseCoNLLU :: String -> Maybe CoNLLUData
parseCoNLLU ss = parseCoNLLUSentenses d $ lines ss
    where
        d = CoNLLUData {
                fileName = "test",
                sentences = [],
                fullWords = initialIndex,
                initialWords = initialIndex,
                uPOSTags = initialIndex,
                xPOSTags = initialIndex,
                featureNames = M.empty,
                featureValues = M.empty,
                depNames = M.empty,
                depRelNames = M.empty ,
                logs = ""
            }
        initialIndex = M.fromList $ zip tagsPredefined [startTagIndex .. endTagIndex]

parseCoNLLUSentenses :: CoNLLUData -> [String] -> Maybe CoNLLUData
parseCoNLLUSentenses d ss = foldM parseCoNLLUSentense d $ filter notNull $ split null ss

parseCoNLLUSentense :: CoNLLUData -> [String] -> Maybe CoNLLUData
parseCoNLLUSentense d ss = case (lookup "text" params, parseWords d wordsLines) of
    (Just t, Just (d', is)) -> return d' {
        sentences = CoNLLUSentense {
            text = trim $ dropPrefix "=" t,
            items = serviceWord startTagIndex 0 : reverse (serviceWord endTagIndex (length is) : is)
            } : sentences d'
        }
    (_, _) -> return d
    where
        (header, wordsLines) = span ("#" `isPrefixOf`) ss
        params = map (mapTuple2 trim . span (/= '=') . dropPrefix "#") header
        serviceWord ix pos = CoNLLUWord {
                wordId = (pos, pos),
                word = ix,
                initialWord = ix,
                uposTag = ix,
                xposTag = ix,
                features = [],
                depHead = -1,
                depRel = ix,
                misc = (tagsPredefined !! ix)
            }

mapTuple2 f (a,b) = (f a, f b)

parseWords :: CoNLLUData -> [String] -> Maybe (CoNLLUData, [CoNLLUWord])
parseWords d = foldM parseWord (d, [])

parseWord :: (CoNLLUData, [CoNLLUWord]) -> String -> Maybe (CoNLLUData, [CoNLLUWord])
parseWord (d, ws) s = do
    return (
        d {fullWords = fws, initialWords = iws, uPOSTags = upts, xPOSTags = xpts, featureNames = fnis, featureValues = fvis, depNames = dns},
        CoNLLUWord {
                wordId = (read wid1, -1),
                word = wIdx,
                initialWord = iwIdx,
                uposTag = optIdx,
                xposTag = xptIdx,
                features = ifs,
                depHead = (read dp),
                depRel = dnIdx,
                misc = (unwords misc)
            }: ws
        )
    where
     --  (n1-n2) idx POS  XPOS features parent role
        (wid: w: iw: opt: xpt: fs:      dp:    drole: _: misc) = split (== '\t') s
        (wid1:wid2) = split (== '-') wid
        (wIdx, fws) = updateWord2IndexWith (fullWords d) w
        (iwIdx, iws) = updateWord2IndexWith (initialWords d) iw
        (optIdx, upts) = updateWord2IndexWith (uPOSTags d) opt
        (xptIdx, xpts)  = updateWord2IndexWith (xPOSTags d) xpt
        (ifs, fnis, fvis) = parseFeatures (featureNames d, featureValues d) fs
        (dnIdx, dns)  = updateWord2IndexWith (depNames d) drole

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

tagsPredefined = ["<-start->", "<-end->"]
startTagIndex = 0
endTagIndex = 1
