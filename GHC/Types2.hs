{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Types2
  ( module GHC.Types2
  , T.Bool(..)
  , T.Coercible(..)
  ) where

import GHC.Prim2

import qualified GHC.Types as T
-- import GHC.Types (Bool(..), Char) as T

data Char = C# Char#

data Int = I# Int#

data Word = W# Word#

data Float = F# Float#

data Double = D# Double#

data Ordering = LT | EQ | GT

isTrue# :: T.Bool -> T.Bool
isTrue# b = b

char2char :: T.Char -> Char
char2char = char2char

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

data RealWorld = RealWorld
data LiftedRep = LiftedRep