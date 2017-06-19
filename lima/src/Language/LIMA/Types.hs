-- |
-- Module      :  Language.LIMA.Types
-- Copyright   :  Galois Inc. 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Collect common types for the LIMA DSL and code generator.
--
module Language.LIMA.Types (
    UID
  , Name
  , Path
  , Phase(..)
) where


type UID = Int

-- | A name.
type Name = String

-- | A hierarchical name.
type Path = [Name]

-- | A phase is either the minimum phase or the exact phase.
data Phase = MinPhase Int | ExactPhase Int
  deriving (Show)
