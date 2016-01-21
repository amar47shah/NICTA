{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

-- Remove these
import Course.Applicative
import Course.Monad

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
--
-- >> anagrams "ASP" "share/threes.txt"
-- ["ASP","PAS","SAP","SPA"]
--
anagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
anagrams cs filename =
  lines <$> readFile filename >>=
  return . intersectBy (==) (permutations cs)

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase =
  error "todo: Course.Anagrams#equalIgnoringCase"
