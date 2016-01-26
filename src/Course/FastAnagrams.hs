{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Map as M

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
counts :: Chars -> M.Map Char Int
counts = foldRight (\c -> M.insertWith (+) c 1) M.empty

match :: Chars -> Chars -> Bool
match = (==) `on` counts

fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams cs filename =
  filter (match cs) . lines <$> readFile filename

-- Ignoring this:

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
