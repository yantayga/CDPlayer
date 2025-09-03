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
