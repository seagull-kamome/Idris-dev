{-# LANGUAGE DeriveDataTypeable #-}
--
--
--
module Idris.Core.SourcePos (
  FC (..),
  FC' (..),
  emptyFC,
  fileFC
  ) where

import Data.Data
import Data.Typeable

-- | Source location. These are typically produced by the parser 'Idris.Parser.getFC'
data FC = FC { fc_fname :: String, -- ^ Filename
               fc_start :: (Int, Int), -- ^ Line and column numbers for the start of the location span
               fc_end :: (Int, Int) -- ^ Line and column numbers for the end of the location span
             }
          deriving (Data, Typeable)
-- | Ignore source location equality (so deriving classes do not compare FCs)
instance Eq FC where
  _ == _ = True

-- | FC with equality
newtype FC' = FC' { unwrapFC :: FC }

instance Eq FC' where
  FC' fc == FC' fc' = fcEq fc fc'
    where fcEq (FC n s e) (FC n' s' e') = n == n' && s == s' && e == e'

-- | Empty source location
emptyFC :: FC
emptyFC = fileFC ""

-- | Source location with file only
fileFC :: String -> FC
fileFC s = FC s (0, 0) (0, 0)


instance Show FC where
    show (FC f s e) = f ++ ":" ++ showLC s e
      where showLC (sl, sc) (el, ec)
              | sl == el && sc == ec = show sl ++ ":" ++ show sc
              | sl == el             = show sl ++ ":" ++ show sc ++ "-" ++ show ec
              | otherwise            = show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec
