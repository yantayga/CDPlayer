{-# LANGUAGE CApiFFI #-}

module CoNLLU.CoNLLU where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils

type DBHandle = Ptr ()

foreign import capi "CoNLLUci.h initCoNLLUDB" initCoNLLUDB :: IO DBHandle
foreign import capi "CoNLLUci.h clearCoNLLUDB" clearCoNLLUDB :: DBHandle -> IO ()
foreign import capi "CoNLLUci.h loadFile" loadFile' :: DBHandle -> CString -> IO CBool
foreign import capi "CoNLLUci.h loadBinary" loadBinary' :: DBHandle -> CString -> CBool -> IO CBool
foreign import capi "CoNLLUci.h saveBinary" saveBinary' :: DBHandle -> CString -> CBool -> IO CBool
foreign import capi "CoNLLUci.h loadDirectory" loadDirectory' :: DBHandle -> CString -> IO CBool
foreign import capi "CoNLLUci.h index2word" index2word' :: DBHandle -> CULong -> IO (CString)
foreign import capi "CoNLLUci.h word2index" word2index' :: DBHandle -> CString -> IO CULong
foreign import capi "CoNLLUci.h wordsCount" wordsCount' :: DBHandle -> IO CULong

loadFile :: DBHandle -> FilePath -> IO Bool
loadFile h path = do
    cpath <- newCString path
    res <- loadFile' h cpath
    return $ toBool res

loadBinary :: DBHandle -> FilePath -> Bool -> IO Bool
loadBinary h path useSentences = do
    cpath <- newCString path
    res <- loadBinary' h cpath (fromBool useSentences)
    return $ toBool res

saveBinary :: DBHandle -> FilePath -> Bool -> IO Bool
saveBinary h path useSentences = do
    cpath <- newCString path
    res <- saveBinary' h cpath (fromBool useSentences)
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

wordsCount :: DBHandle -> IO Integer
wordsCount h = do
    n <- wordsCount' h
    return $ toInteger n
