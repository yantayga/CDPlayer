module CD where

import Text.Parsec hiding (State)
import CDDB.Types
import CDDB.Parser

parseCDDBFile :: FilePath -> IO (Either ParseError CDDB)
parseCDDBFile path = do
    fileContent <- readFile path
    let res = parseCDDB path fileContent
    return res

