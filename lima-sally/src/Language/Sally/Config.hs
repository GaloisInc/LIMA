-- |
-- Module      :  Language.Sally.Config
-- Copyright   :  Galois Inc. 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Translation configuration, including settings for the fault model,
-- the way variables are rendered, etc.
--
module Language.Sally.Config
  ( TrConfig(..)
  , FaultAssump(..)
  , defaultCfg
  , hybridMFA
  , Weights
  ) where

import Language.Sally.FaultModel
import Language.Sally.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- | Translation configuration, including settings for the fault model.
data TrConfig = TrConfig
  { -- | maximum fault assumption / fault model to use
    cfgMFA :: FaultAssump
    -- | use top-level name space in variables names?
    --   TODO this doesn't work currently because of
    --   the way Atom generates names during
    --   'elaborate'
  , cfgTopNameSpace :: Bool
    -- | turn debugging output on, causes extra variables and
    --   transitions to be generated that aid in
    --   debugging
  , cfgDebug :: Bool
  , cfgMessageDelay :: Rational
  }

-- | Default configuration
defaultCfg :: TrConfig
defaultCfg = TrConfig
  { cfgMFA = NoFaults
  , cfgTopNameSpace = True
  , cfgDebug = False
  , cfgMessageDelay = 1
  }

-- | Assignment of weights to each fault type
type Weights = Map FaultType Int

-- | Type representing possible fault model assumptions
data FaultAssump =
  -- | No faulty nodes
    NoFaults
  -- | Hybrid faults with weights and a constant term
  -- TODO: elaborate
  | HybridFaults Weights Int
  -- | Fixed configuration of faulty nodes. Nodes not specified are assigned
  -- 'NonFaulty'.
  | FixedFaults (Map Name FaultType)

-- | An example fault type weighting that is appropriate for systems like
-- OM(1).
hybridMFA :: FaultAssump
hybridMFA =
  let ws = [ (NonFaulty,       0)
           , (ManifestFaulty , 1)
           , (SymmetricFaulty, 2)
           , (ByzantineFaulty, 3)
           ]
  in HybridFaults (Map.fromList ws) 1
