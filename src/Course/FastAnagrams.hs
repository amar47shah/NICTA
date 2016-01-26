{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Map as M

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
counts :: NoCaseString -> M.Map Char Int
counts = foldRight count M.empty . uncased
    where
  count c = M.insertWith (+) c 1

match :: Chars -> Chars -> Bool
match = (==) `on` counts . NoCaseString

fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams cs filename =
  filter (match cs) . lines <$> readFile filename

class Case a where
  noCase :: a -> a

instance Case Char where
  noCase = toLower

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` uncased

instance Show NoCaseString where
  show = show . uncased

uncased :: NoCaseString -> Chars
uncased = map noCase . ncString
