{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Interactive where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Traversable
import Course.List
import Course.Optional

-- | Eliminates any value over which a functor is defined.
vooid ::
  Functor m =>
  m a
  -> m ()
vooid =
  (const () <$>)

-- | A version of @bind@ that ignores the result of the effect.
(>-) ::
  Monad m =>
  m a
  -> m b
  -> m b
(>-) a =
  (a >>=) . const

-- | Runs an action until a result of that action satisfies a given predicate.
untilM ::
  Monad m =>
  (a -> m Bool) -- ^ The predicate to satisfy to stop running the action.
  -> m a -- ^ The action to run until the predicate satisfies.
  -> m a
untilM p a =
  a >>= \r ->
    p r >>= depends (return r) (untilM p a)

quitOnQ :: Char -> IO Bool
quitOnQ = depends (putStrLn "Bye!" >- return True) (return False) . (== 'q')

-- | Example program that uses IO to echo back characters that are entered by the user.
echo ::
  IO ()
echo =
  vooid
    . untilM quitOnQ
    $ putStr "Enter a character: " >-
      getChar >>=
      \c -> putStrLn ('\n' :. c :. Nil) >- return c

data Op =
  Op Char Chars (IO ()) -- keyboard entry, description, program

-- |
--
-- * Ask the user to enter a string to convert to upper-case.
--
-- * Convert the string to upper-case.
--
-- * Print the upper-cased string to standard output.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @toUpper :: Char -> Char@ -- (Data.Char) converts a character to upper-case.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
convertInteractive ::
  IO ()
convertInteractive =
  putStr "Enter a string: " >- getLine >>= putStrLn . (toUpper <$>)

-- |
--
-- * Ask the user to enter a file name to reverse.
--
-- * Ask the user to enter a file name to write the reversed file to.
--
-- * Read the contents of the input file.
--
-- * Reverse the contents of the input file.
--
-- * Write the reversed contents to the output file.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @readFile :: FilePath -> IO String@ -- an IO action that reads contents of a file.
--
-- /Tip:/ @writeFile :: FilePath -> String -> IO ()@ -- writes a string to a file.
--
-- /Tip:/ @reverse :: [a] -> [a]@ -- reverses a list.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
reverseInteractive ::
  IO ()
reverseInteractive = do
  source <- putStr "Enter a source file: " >> getLine
  target <- putStr "Enter a target file: " >> getLine
  readFile source >>= writeFile target . reverse

-- |
--
-- * Ask the user to enter a string to url-encode.
--
-- * Convert the string with a URL encoder.
--
-- * For simplicity, encoding is defined as:
--
-- * @' ' -> \"%20\"@
--
-- * @'\t' -> \"%09\"@
--
-- * @'\"' -> \"%22\"@
--
-- * @/anything else is unchanged/@
--
-- * Print the encoded URL to standard output.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
encodeInteractive ::
  IO ()
encodeInteractive =
  let encode = foldRight acc ""
      acc ' '  = ("%20" ++)
      acc '\t' = ("%09" ++)
      acc '\"' = ("%22" ++)
      acc c    = (c :.)
   in putStr "Enter a URL: " >- getLine >>= putStrLn . encode

interactive ::
  IO ()
interactive =
  let ops =  Op 'c' "Convert a string to upper-case" convertInteractive
          :. Op 'r' "Reverse a file" reverseInteractive
          :. Op 'e' "Encode a URL" encodeInteractive
          :. Op 'q' "Quit" (pure ())
          :. Nil
      printOp (Op c s _) = putStr (c :. Nil) >- putStr ". " >- putStrLn s
      match c (Op d _ _) = c == d
      handle (Full (Op _ _ k)) = (k >-)
      handle _                 = (putStrLn "Not a valid selection. Try again." >-)
  in vooid
     . untilM quitOnQ
     $ putStrLn "Select: " >-
       traverse printOp ops >-
       getChar >>=
       \c -> (putStrLn "" >-) . handle (find (match c) ops) $ return c
