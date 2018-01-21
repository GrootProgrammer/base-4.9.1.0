{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Magic2
  ( module GHC.Magic2
--   , module GHC.Magic
  ) where

-- import GHC.Magic

oneShot :: (a -> b) -> (a -> b)
oneShot f = f

