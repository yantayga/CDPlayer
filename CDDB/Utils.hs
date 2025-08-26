{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Utils where

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b) 
