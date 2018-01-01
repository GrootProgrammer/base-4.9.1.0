{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module GHC.Prim2
  ( module GHC.Prim2
  , module GHC.Prim
  ) where

import GHC.Types2 (Bool)
import GHC.Prim hiding
    ((+#), (-#), (*#), negateInt#,
     (==#), (/=#), (>#), (>=#), (<#), (<=#),

     (==##), (/=##), (>##), (>=##), (<##), (<=##),

     eqFloat#, gtFloat#, geFloat#, ltFloat#, leFloat#,
     negateFloat#,
     plusFloat#, minusFloat#, timesFloat#, divideFloat#,
     powerFloat#, sqrtFloat#,

     eqChar#, neChar#, gtChar#, geChar#, ltChar#, leChar#,
     eqWord#, neWord#, gtWord#, geWord#, ltWord#, leWord#,
     word2Int#
     )

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

-- Double#

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

powerFloat# :: Float# -> Float# -> Float#
powerFloat# = powerFloat#

sqrtFloat# :: Float# -> Float#
sqrtFloat# = sqrtFloat#

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

