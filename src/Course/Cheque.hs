{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded)

instance Show Digit where
  show = show . ("Digit " ++) . showDigit

showDigit :: Digit -> Chars
showDigit Zero  = "zero"
showDigit One   = "one"
showDigit Two   = "two"
showDigit Three = "three"
showDigit Four  = "four"
showDigit Five  = "five"
showDigit Six   = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine  = "nine"

showDigit3 :: Digit3 -> Chars
showDigit3 (D1           o) = showDigit o
showDigit3 (D2      Zero o) = showDigit3 $ D1 o
showDigit3 (D2      t    o) = underHundred t o
showDigit3 (D3 Zero t    o) = showDigit3 $ D2 t o
showDigit3 (D3 h    t    o) = overHundred h t o

overHundred :: Digit -> Digit -> Digit -> Chars
overHundred h Zero Zero = hundreds h
overHundred h t o       = hundreds h ++ " and " ++ underHundred t o

hundreds :: Digit -> Chars
hundreds h = showDigit h ++ " hundred"

underHundred :: Digit -> Digit -> Chars
underHundred Zero o     = showDigit o
underHundred One  Zero  = "ten"
underHundred One  One   = "eleven"
underHundred One  Two   = "twelve"
underHundred One  Three = "thirteen"
underHundred One  Four  = "fourteen"
underHundred One  Five  = "fifteen"
underHundred One  Six   = "sixteen"
underHundred One  Seven = "seventeen"
underHundred One  Eight = "eighteen"
underHundred One  Nine  = "nineteen"
underHundred t    Zero  = tens t
underHundred t    o     = tens t ++ '-' :. showDigit o

tens :: Digit -> Chars
tens Two   = "twenty"
tens Three = "thirty"
tens Four  = "forty"
tens Five  = "fifty"
tens Six   = "sixty"
tens Seven = "seventy"
tens Eight = "eighty"
tens Nine  = "ninety"
tens _     = ""

isZero :: Digit3 -> Bool
isZero (D1 Zero)           = True
isZero (D2 Zero Zero)      = True
isZero (D3 Zero Zero Zero) = True
isZero _                   = False

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar  _  = Empty

allowables :: Chars
allowables = fromString "0123456789."

dollarsAndCents :: Chars -> (Chars, Chars)
dollarsAndCents = dollarsAndCents' . filter (`elem` allowables)

dollarsAndCents' :: Chars -> (Chars, Chars)
dollarsAndCents' = (dols *** cnts) . break (== '.')
    where
  dols, cnts, cnts' :: Chars -> Chars
  dols "" = "0"
  dols ds = ds
  cnts    = cnts' . filter (/= '.')
  cnts' (t :. h :. _) = t :. h :. ""
  cnts' (t :. _)      = t :. "0"
  cnts' _             = "00"

onlyDollars :: Chars -> Chars
onlyDollars cs =
  let digs  = (sequence $ fromChar <$> cs) ?? listh [Zero]
      dig3s = case length digs `mod` 3 of
                2 -> let (t:.o:._, ds) = splitAt 2 digs in D2 t o :. groupD3 ds
                1 -> let    (o:._, ds) = splitAt 1 digs in D1   o :. groupD3 ds
                _ -> groupD3 digs
      groupD3 = map (\(h:.t:.o:.Nil) -> D3 h t o) . splitEvery 3
      illionize (d3, "") = showDigit3 d3
      illionize (d3, il) = showDigit3 d3 ++ ' ' :. il
      i :. is = zip (reverse dig3s) illion
   in intercalate " " . reverse
    $ illionize <$> i :. filter (not . isZero . fst) is

onlyCents :: Chars -> Chars
onlyCents cs =
  let t :. o :. _ = (sequence $ fromChar <$> cs) ?? listh [Zero, Zero]
   in underHundred t o

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars = together
        . (plural "dollar" *** plural "cent")
        . (onlyDollars *** onlyCents)
        . dollarsAndCents
  where together :: (Chars, Chars) -> Chars
        together (d, c) = d ++ " and " ++ c
        plural :: Chars -> Chars -> Chars
        plural noun "one"  = "one " ++ noun
        plural noun number = number ++ " " ++ noun ++ "s"
