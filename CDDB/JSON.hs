module CDDB.JSON where

import Data.Aeson (Value(..))
import Text.Read (readMaybe)
import Data.Text (unpack)

import Control.Applicative (empty)

tryParseJSON (String s) = case (readMaybe $ unpack s) of
    Nothing -> empty
    Just t -> return t
tryParseJSON _ = empty