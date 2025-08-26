module CDDB.JSON where

import Data.Aeson (Value(..))
import Text.Read (readMaybe)
import Data.Text (unpack)

import Control.Applicative (empty)

tryParseJSON (String s) = maybe empty return (readMaybe $ unpack s)
tryParseJSON _ = empty
