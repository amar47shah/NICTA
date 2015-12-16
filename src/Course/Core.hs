{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Core(
  Eq(..)
, Ord(..)
, Show(..)
, Enum(..)
, Integral(..)
, Bounded(..)
, RealFrac(..)
, Num(..)
, Fractional(..)
, Bool(..)
, Either(..)
, Int
, Integer
, IO
, Rational
, seq
, error
, undefined
, const
, flip
, curry
, uncurry
, id
, otherwise
, (.)
, ($)
, (&&)
, (||)
, not
, even
, odd
, fst
, snd
, getChar
, on
, first
, second
, (&&&)
, (***)
, IsString(..)
, module Data.Char
, ifThenElse
, depends
) where


import Prelude(
    Eq(..)
  , Ord(..)
  , Show(..)
  , Enum(..)
  , Integral(..)
  , Bounded(..)
  , RealFrac(..)
  , Num(..)
  , Fractional(..)
  , Bool(..)
  , Either(..)
  , Int
  , Integer
  , IO
  , Rational
  , seq
  , error
  , undefined
  , const
  , flip
  , curry
  , uncurry
  , id
  , otherwise
  , (.)
  , ($)
  , (&&)
  , (||)
  , not
  , even
  , odd
  , fst
  , snd
  )
import Data.String(
  IsString(..)
  )

import System.IO(
    getChar
  )
import Data.Function(
    on
  )
import Control.Arrow(
    first
  , second
  , (&&&)
  , (***)
  )
import Data.Char

ifThenElse ::
  Bool
  -> a
  -> a
  -> a
ifThenElse True t _ =
  t
ifThenElse False _ f =
  f

-- depends === curry . flip $ uncurry . ifThenElse
depends ::
  a
  -> a
  -> Bool
  -> a
depends v _ True  = v
depends _ w False = w
