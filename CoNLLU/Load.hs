{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}

module CoNLLU.Load where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (combine)

import Control.Monad (foldM)

import CoNLLU.Types
import CoNLLU.Parse

loadCoNLLU :: CoNLLUData -> FilePath -> IO (Maybe CoNLLUData)
loadCoNLLU db fn = do
    content <- TIO.readFile fn
    return $ parseCoNLLU (db {fileName = T.pack fn}) content

loadDirectory :: CoNLLUData -> FilePath -> IO ([T.Text], CoNLLUData)
loadDirectory db fp = do
    fs <- getFiles fp
    foldM loadCoNLLUWithLog (emptyLog, db) fs
    where
        emptyLog = [""]
        loadCoNLLUWithLog (logs, db) fn = do
            maybeDB <- loadCoNLLU db fn
            return $ case maybeDB of
                Nothing -> (T.pack ("Failed to load " ++ fn): logs, db)
                Just db' -> (logs, db')
        getFiles fp  = do
            fp' <- canonicalizePath fp
            ed <- doesDirectoryExist fp'
            ef <- doesFileExist fp'
            if ed
            then do
                fs <- listDirectory fp'
                sfs <- mapM (getFiles . combine fp') fs
                return $ concat sfs
            else if ef then return [fp'] else return []
