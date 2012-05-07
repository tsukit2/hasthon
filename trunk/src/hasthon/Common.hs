{-# LANGUAGE TupleSections #-}
module Hasthon.Common where

import Text.PrettyPrint

class PrettyPrintable a where
   toPrettyDoc :: a -> Doc


