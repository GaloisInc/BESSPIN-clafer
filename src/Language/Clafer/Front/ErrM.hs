-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module Language.Clafer.Front.ErrM where

import Language.Clafer.Front.Absclafer

-- the Error monad: like Maybe type with error msgs

import Control.Monad (MonadPlus(..), liftM)

data Err a = Ok a | Bad Pos String
  deriving (Show, Eq, Ord)

instance Monad Err where
  return      = Ok
  fail        = Bad noPos
  Ok a  >>= f = f a
  Bad p s >>= _ = Bad p s

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad noPos "Err.mzero"
  mplus (Bad _ _) y = y
  mplus x       _ = x
