{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module GHC.Prim2
  ( module GHC.Prim2
--   , module GHC.Prim
  , Int#, Double#, Char#, Float#, Word#, TYPE
  , coerce
  ) where

import GHC.Prim
  ( Int#, Double#, Char#, Float#, Word#, TYPE -- , Addr#
  , coerce)

import GHC.Types
  (Bool, Char)

-- import GHC.Prim hiding
--     ((+#), (-#), (*#), negateInt#,
--      (==#), (/=#), (>#), (>=#), (<#), (<=#),
-- 
--      (==##), (/=##), (>##), (>=##), (<##), (<=##),
-- 
--      eqFloat#, gtFloat#, geFloat#, ltFloat#, leFloat#,
--      negateFloat#,
--      plusFloat#, minusFloat#, timesFloat#, divideFloat#,
--      powerFloat#, sqrtFloat#, expFloat#, logFloat#,
--      sinFloat#, cosFloat#, tanFloat#,
--      asinFloat#, acosFloat#, atanFloat#,
--      sinhFloat#, coshFloat#, tanhFloat#,
-- 
--      eqChar#, neChar#, gtChar#, geChar#, ltChar#, leChar#,
--      eqWord#, neWord#, gtWord#, geWord#, ltWord#, leWord#,
--      word2Int#
--     )

-- Int# operators

(+#) :: Int# -> Int# -> Int#
(+#) = (+#)

(-#) :: Int# -> Int# -> Int#
(-#) = (-#)

(*#) :: Int# -> Int# -> Int#
(*#) = (*#)

negateInt# :: Int# -> Int#
negateInt# = negateInt#

-- Int#

(==#) :: Int# -> Int# -> Bool
(==#) = (==#)

(/=#) :: Int# -> Int# -> Bool
(/=#) = (/=#)

(>#) :: Int# -> Int# -> Bool
(>#) = (>#)

(>=#) :: Int# -> Int# -> Bool
(>=#) = (>=#)

(<#) :: Int# -> Int# -> Bool
(<#) = (<#)

(<=#) :: Int# -> Int# -> Bool
(<=#) = (<=#)

quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
quotRemInt# x y = (# quotInt# x y, remInt# x y #)

quotInt# :: Int# -> Int# -> Int#
quotInt# = quotInt#

remInt# :: Int# -> Int# -> Int#
remInt# = remInt#

-- Double#

(+##) :: Double# -> Double# -> Double#
(+##) = (+##)

(-##) :: Double# -> Double# -> Double#
(-##) = (-##)

(*##) :: Double# -> Double# -> Double#
(*##) = (*##)

(/##) :: Double# -> Double# -> Double#
(/##) = (/##)

(==##) :: Double# -> Double# -> Bool
(==##) = (==##)

(/=##) :: Double# -> Double# -> Bool
(/=##) = (/=##)

(>##) :: Double# -> Double# -> Bool
(>##) = (>##)

(>=##) :: Double# -> Double# -> Bool
(>=##) = (>=##)

(<##) :: Double# -> Double# -> Bool
(<##) = (<##)

(<=##) :: Double# -> Double# -> Bool
(<=##) = (<=##)

negateDouble# :: Double# -> Double#
negateDouble# = negateDouble#

-- Float#

eqFloat# :: Float# -> Float# -> Bool
eqFloat# = eqFloat#

gtFloat# :: Float# -> Float# -> Bool
gtFloat# = gtFloat#

geFloat# :: Float# -> Float# -> Bool
geFloat# = geFloat#

ltFloat# :: Float# -> Float# -> Bool
ltFloat# = ltFloat#

leFloat# :: Float# -> Float# -> Bool
leFloat# = leFloat#

negateFloat# :: Float# -> Float#
negateFloat# = negateFloat#

plusFloat# :: Float# -> Float# -> Float#
plusFloat# = plusFloat#

minusFloat# :: Float# -> Float# -> Float#
minusFloat# = minusFloat#

timesFloat# :: Float# -> Float# -> Float#
timesFloat# = timesFloat#

divideFloat# :: Float# -> Float# -> Float#
divideFloat# = divideFloat#

-- powerFloat# :: Float# -> Float# -> Float#
-- powerFloat# = powerFloat#
-- 
-- sqrtFloat# :: Float# -> Float#
-- sqrtFloat# = sqrtFloat#
-- 
-- expFloat# :: Float# -> Float#
-- expFloat# = expFloat#
-- 
-- logFloat# :: Float# -> Float#
-- logFloat# = logFloat#
-- 
-- sinFloat# :: Float# -> Float#
-- sinFloat# = sinFloat#
-- 
-- cosFloat# :: Float# -> Float#
-- cosFloat# = cosFloat#
-- 
-- tanFloat# :: Float# -> Float#
-- tanFloat# = tanFloat#
-- 
-- asinFloat# :: Float# -> Float#
-- asinFloat# = asinFloat#
-- 
-- acosFloat# :: Float# -> Float#
-- acosFloat# = acosFloat#
-- 
-- atanFloat# :: Float# -> Float#
-- atanFloat# = atanFloat#
-- 
-- sinhFloat# :: Float# -> Float#
-- sinhFloat# = sinhFloat#
-- 
-- coshFloat# :: Float# -> Float#
-- coshFloat# = coshFloat#
-- 
-- tanhFloat# :: Float# -> Float#
-- tanhFloat# = tanhFloat#

-- Char#

eqChar# :: Char# -> Char# -> Bool
eqChar# = eqChar#

neChar# :: Char# -> Char# -> Bool
neChar# = neChar#

gtChar# :: Char# -> Char# -> Bool
gtChar# = gtChar#

geChar# :: Char# -> Char# -> Bool
geChar# = geChar#

ltChar# :: Char# -> Char# -> Bool
ltChar# = ltChar#

leChar# :: Char# -> Char# -> Bool
leChar# = leChar#

ord# :: Char# -> Int#
ord# = ord#

-- Word#

eqWord# :: Word# -> Word# -> Bool
eqWord# = eqWord#

neWord# :: Word# -> Word# -> Bool
neWord# = neWord#

gtWord# :: Word# -> Word# -> Bool
gtWord# = gtWord#

geWord# :: Word# -> Word# -> Bool
geWord# = geWord#

ltWord# :: Word# -> Word# -> Bool
ltWord# = ltWord#

leWord# :: Word# -> Word# -> Bool
leWord# = leWord#

word2Int# :: Word# -> Int#
word2Int# = word2Int#

int2Word# :: Int# -> Word#
int2Word# = int2Word#

-- Others

seq :: a -> b -> b
seq _ b = b  -- Anton: This is technically wrong.

-- Hack to get a SymGen somewhere in base
symgen :: a
symgen = symgen

-- Misc add-ons

fromIntToFloat :: Int# -> Float#
fromIntToFloat = fromIntToFloat

fromIntToDouble :: Int# -> Double#
fromIntToDouble = fromIntToDouble

data State# s = State# s

data Void# = Void#

void# :: Void#
void# = Void#

data Addr# = Addr# Int#

nullAddr# :: Addr#
nullAddr# = Addr# 0#

plusAddr# :: Addr# -> Int# -> Addr# 
plusAddr# (Addr# x) y= Addr# (x +# y)

minusAddr# :: Addr# -> Addr# -> Int# 
minusAddr# (Addr# x) (Addr# y) = (x -# y)
