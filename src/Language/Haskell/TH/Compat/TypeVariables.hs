{-# LANGUAGE CPP #-}

-- |
-- Module      : Language.Haskell.TH.Compat.TypeVariables
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides compatibility definitions to calculate
-- free type variables for before temaplate-haskell-2.10
module Language.Haskell.TH.Compat.TypeVariables (
  fromType
  ) where

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Compat.TypeVariables.Current
#else
import Language.Haskell.TH.Compat.TypeVariables.V210
#endif
