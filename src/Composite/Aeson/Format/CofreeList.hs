{- |
   Module     : Composite.Aeson.Format.CofreeList
   License    : MIT
   Stability  : experimental

Format a Cofree [] as a JSON value with "head", "tail" keys.
-}
{-# LANGUAGE OverloadedStrings #-}
module Composite.Aeson.Format.CofreeList (
  cofreeListJsonFormat
) where

import Composite.Aeson
import Composite.Aeson.WriteOnly
import Control.Comonad.Cofree
import Data.Aeson as A
import Data.Vector as V

cofreeListJsonFormat :: JsonFormat e a -> JsonFormat e (Cofree [] a)
cofreeListJsonFormat f = writeOnlyJsonFormat p where
                           p = \(x :< xs) -> object $ ["head" A..= toJsonWithFormat f x] <> (
                                                  case xs of
                                                    [] -> []
                                                    _  -> ["tail" A..= Array (V.fromList $ p <$> xs) ]
                                                   )
