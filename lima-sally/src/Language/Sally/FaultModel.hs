module Language.Sally.FaultModel
  ( FaultType(..)
  , faultTypeMin
  , faultTypeMax
  )
where

import Language.Sally.Types


-- | Enumerate the types of faults that a node can have in our model.
data FaultType = NonFaulty
               | ManifestFaulty
               | SymmetricFaulty
               | ByzantineFaulty
  deriving (Eq, Ord, Show, Enum, Bounded)

faultTypeMin :: Int
faultTypeMin = fromEnum (minBound :: FaultType)

faultTypeMax :: Int
faultTypeMax = fromEnum (maxBound :: FaultType)

-- | Turn fault types into integers for the Sally encoding
instance ToSallyExpr FaultType where
  toSallyExpr = SELit . SConstInt . fromIntegral . fromEnum
