{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Language.AG.Contraint
-- Description : Utilities related to 'Contraint's
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : BSD-3
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
-- Portability : GHC
module Language.AG.Constraint
  ( -- A utility typeclass that acts as a logical *and* for types of kind @'Type' -> 'Contraint'@.
    --
    -- @
    -- type OrdShow = Show && Ord
    --
    -- printMin :: OrdShow a => a -> a -> IO ()
    -- printMin x y = if x <= y then print x else print y
    -- @
    type (&&&)
  ) where

import Data.Kind

class (a c, b c) => (&&&) (a :: Type -> Constraint) (b :: Type -> Constraint) c
infixl 4 &&&

instance (a c, b c) => (a &&& b) c
