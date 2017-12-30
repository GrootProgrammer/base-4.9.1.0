{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Types2
  ( module GHC.Types2,
    module GHC.Types
  ) where

import GHC.Types hiding (isTrue#)

isTrue# :: Bool -> Bool
isTrue# b = b

