{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
-- Reading colluu data
-- https://universaldependencies.org/format.html
module CoNLLU.Parse where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Map.Strict as M
import qualified Data.Vector.Strict as V
import Data.List (group)
import Data.List.Extra (split, replace)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.Char (isDigit)

import Control.Monad (foldM)
import Control.DeepSeq

import CoNLLU.Types

emptyDB :: CoNLLUData
emptyDB = CoNLLUData {
                fileName = "",
                sentences = V.empty,
                fullWords = initialIndex,
                initialWords = initialIndex,
                uPOSTags = initialIndex,
                xPOSTags = initialIndex,
                featureNames = M.empty,
                featureValues = M.empty,
                depNames = M.empty,
                depRelNames = M.empty,
                startWord = snd startTag,
                endWord = snd endTag
            }
    where
        initialIndex = M.fromList tagsPredefined

startTag = ("<-start->", 0)
endTag = ("<-end->", 1)
tagsPredefined = [startTag, endTag]

parseCoNLLU :: CoNLLUData -> T.Text -> Maybe CoNLLUData
parseCoNLLU d ss = parseCoNLLUSentenses d $ T.lines ss

parseCoNLLUSentenses :: CoNLLUData -> [T.Text] -> Maybe CoNLLUData
parseCoNLLUSentenses d ss = foldM parseCoNLLUSentense d $ filter (not . null) $ split T.null ss

parseCoNLLUSentense :: CoNLLUData -> [T.Text] -> Maybe CoNLLUData
parseCoNLLUSentense d ss = force $ case parseWords d wordsLines of
    Just (d', !is) -> return d' {
        sentences = V.cons (CoNLLUSentense {
            text = T.strip $ dropTextPrefix "=" $ fromMaybe "" (lookup "text" params),
            items = V.fromList $ reverse is
            }) (sentences d')
        }
    Nothing -> return d
    where
        (header, wordsLines) = span ("#" `T.isPrefixOf`) ss
        params = map (mapTuple2 T.strip . T.span (/= '=') . dropTextPrefix "#") header

mapTuple2 f (a,b) = (f a, f b)

parseWords :: CoNLLUData -> [T.Text] -> Maybe (CoNLLUData, [CoNLLUWord])
parseWords d = foldM parseWord (d, [])

parseWord :: (CoNLLUData, [CoNLLUWord]) -> T.Text -> Maybe (CoNLLUData, [CoNLLUWord])
parseWord (d, ws) s = if tabsCount < 9 then Just (d, ws) else do
    return (
        d {fullWords = fws, initialWords = iws, uPOSTags = upts, xPOSTags = xpts, featureNames = fnis, featureValues = fvis, depNames = dns}, newWord: ws
        )
    where
        tabsCount = length $ filter (== '\t') $ T.unpack s
     --  (n1-n2) idx POS  XPOS features parent role
        (wid: w: iw: opt: xpt: fs:      dp:    drole: _: misc) = T.split (== '\t') s
        (wid1:wid2) = T.split (== '-') wid
        (wIdx, fws) = updateWord2IndexWith (fullWords d) (filterNums w) id
        (iwIdx, iws) = updateWord2IndexWith (initialWords d) (filterNums iw) id
        (optIdx, upts) = updateWord2IndexWith (uPOSTags d) opt T.toUpper
        (xptIdx, xpts)  = updateWord2IndexWith (xPOSTags d) xpt T.toUpper
        (ifs, fnis, fvis) = parseFeatures (featureNames d, featureValues d) fs
        (dnIdx, dns)  = updateWord2IndexWith (depNames d) drole T.toLower
        newWord = CoNLLUWord {
                wordId = (fst $ fromRight (0,"") $ TR.decimal wid1, -1),
                word = wIdx,
                initialWord = iwIdx,
                uposTag = optIdx,
                xposTag = xptIdx,
                features = ifs,
                depHead = (fst $ fromRight (0,"") $ TR.decimal dp),
                depRel = dnIdx,
                misc = (T.unwords misc)
            }
        filterNums w = T.concat $ replace [""] ["<N>"] $ map head $ group $ T.split isDigit w

parseFeatures :: (Word2Index, Word2Index) -> T.Text -> (Features, Word2Index, Word2Index)
parseFeatures (fnis, fvis) s = foldl' update ([], fnis, fvis) pairs
    where
        pairs = map (T.span (/= '=')) $ T.split (== '|') s
        update :: (Features, Word2Index, Word2Index) -> (T.Text, T.Text) -> (Features, Word2Index, Word2Index)
        update (fs, fnis, fvis) (n, v) =
            let (ni, fnis') = updateWord2IndexWith fnis n T.toLower
                (vi, fvis') = updateWord2IndexWith fvis (dropTextPrefix "=" v) T.toLower
            in ((ni, vi):fs, fnis', fvis')

updateWord2IndexWith :: Word2Index -> T.Text -> (T.Text -> T.Text) -> (WordIndex, Word2Index)
updateWord2IndexWith w2i s f = case M.insertLookupWithKey (\_ _ a -> a) (f s) nextIx w2i of
        (Nothing, newMap)  -> (nextIx, newMap)
        (Just oldval, newMap) -> (oldval, newMap)
    where
        nextIx = M.size w2i

dropTextPrefix p s = fromMaybe s $ T.stripPrefix p s