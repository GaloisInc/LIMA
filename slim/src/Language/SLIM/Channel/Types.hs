module Language.SLIM.Channel.Types (
    ChanInput (..)
  , ChanOutput (..)
  , ChannelDelay(..)
  , mkChanInput
  , mkChanOutput
  , HasChan(..)
) where

import Data.Word (Word64)

import Language.SLIM.Types
import Language.SLIM.Expressions


-- | Input side of a typed channel
data ChanInput = ChanInput
  { cinID   :: Int
  , cinName :: Name
  , cinType :: Type
  }
  deriving (Eq, Show)

mkChanInput :: Int -> Name -> Type -> ChanInput
mkChanInput = ChanInput

-- | Output side of a typed channel
data ChanOutput = ChanOutput
  { coutID   :: Int
  , coutName :: Name
  , coutType :: Type
  }
  deriving (Eq, Show)

mkChanOutput :: Int -> Name -> Type -> ChanOutput
mkChanOutput = ChanOutput

-- | Channel delay specification
data ChannelDelay = DelayDefault
                  | DelayTicks Word64


-- Channel Operations --------------------------------------------------

class HasChan b where
  chanID   :: b -> Int
  chanName :: b -> Name
  chanType :: b -> Type

instance HasChan ChanInput where
  chanID   = cinID
  chanName = cinName
  chanType = cinType

instance HasChan ChanOutput where
  chanID   = coutID
  chanName = coutName
  chanType = coutType
