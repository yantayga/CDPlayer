{-# LANGUAGE CApiFFI #-}

module CoNLLU.CoNLLU where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils

type DBHandle = Ptr ()

foreign import capi "CoNLLUci.h initCoNLLUDB" initCoNLLUDB :: IO DBHandle
foreign import capi "CoNLLUci.h clearCoNLLUDB" clearCoNLLUDB :: DBHandle -> IO ()
foreign import capi "CoNLLUci.h loadFile" loadFile' :: DBHandle -> CString -> IO CBool
foreign import capi "CoNLLUci.h loadDirectory" loadDirectory' :: DBHandle -> CString -> IO CBool
foreign import capi "CoNLLUci.h index2word" index2word' :: DBHandle -> CULong -> IO (CString)
foreign import capi "CoNLLUci.h word2index" word2index' :: DBHandle -> CString -> IO CULong
foreign import capi "CoNLLUci.h index2tag" index2tag' :: DBHandle -> CUShort -> IO (CString)
foreign import capi "CoNLLUci.h tag2index" tag2index' :: DBHandle -> CString -> IO CUShort
foreign import capi "CoNLLUci.h wordsCount" wordsCount' :: DBHandle -> IO CULong
foreign import capi "CoNLLUci.h tagsCount" tagsCount' :: DBHandle -> IO CULong

loadFile :: DBHandle -> FilePath -> IO Bool
loadFile h path = do
    cpath <- newCString path
    res <- loadFile' h cpath
    return $ toBool res

loadDirectory :: DBHandle -> FilePath -> IO Bool
loadDirectory h path = do
    cpath <- newCString path
    res <- loadDirectory' h cpath
    return $ toBool res

index2word :: DBHandle -> Integer -> IO String
index2word h ix = do
    cs <- index2word' h $ fromInteger ix
    peekCString cs

word2index :: DBHandle -> String -> IO Integer
word2index h s = do
    cs <- newCString s
    ix <- word2index' h cs
    return $ toInteger ix

index2tag :: DBHandle -> Integer -> IO String
index2tag h ix = do
    cs <- index2tag' h $ fromInteger ix
    peekCString cs

tag2index :: DBHandle -> String -> IO Integer
tag2index h s = do
    cs <- newCString s
    ix <- word2index' h cs
    return $ toInteger ix

wordsCount :: DBHandle -> IO Integer
wordsCount h = do
    n <- wordsCount' h
    return $ toInteger n

tagsCount :: DBHandle -> IO Integer
tagsCount h = do
    n <- tagsCount' h
    return $ toInteger n
