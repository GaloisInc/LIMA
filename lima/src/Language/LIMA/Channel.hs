-- |
-- Module      :  Language.LIMA.Channel
-- Copyright   :  Galois Inc. 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- First class channels for atom-atom communication. The intention is that
-- channels and operations on them may be translated differently depending on
-- the target. For example in the C code generator they are translated into
-- pairs of (value, ready flag) variables.
--
module Language.LIMA.Channel
  ( -- * Channel Declarations
    channel
    -- * Channel operations
  , writeChannelWithDelay
  , writeChannel
  , readChannel
  , fullChannel
    -- * misc exports
  , channelPrefix
  )
where

import MonadLib

import Language.LIMA.Types
import Language.LIMA.Channel.Types
import Language.LIMA.Elaboration
import Language.LIMA.Expressions
import Language.LIMA.UeMap (newUE)


-- Channel API ------------------------------------------------

-- | Declare a typed channel. Returns channel input/output handles.
channel :: Name  -- ^ channel name
        -> Type  -- ^ type of values in the channel
        -> Atom (ChanInput, ChanOutput)
channel name t = do
  -- add the __chanel_ prefix to the channel name before registering the name
  -- to try to separate the channel and variable namespaces somewhat
  let sName = channelPrefix ++ name
  name' <- addName sName
  (st, (g, atom)) <- get
  let cin  = mkChanInput (gChannelId g) name' t
      cout = mkChanOutput (gChannelId g) name' t
  set (st, ( g { gChannelId = gChannelId g + 1
               , gState = gState g ++ [StateChannel sName t]
               }
           , atom
           )
      )
  return (cin, cout)

-- | Write a message to a typed channel. The write operation happens once
-- (i.e. the last writeChannel in the sequence is used) after the assignment
-- (computation) phase of the Atom's execution.
--
-- The write operation overwrites the content of the given channel.
writeChannelWithDelay :: Expr a => ChannelDelay -> ChanInput -> E a -> Atom ()
writeChannelWithDelay d cin e = do
  (st, (g, atom)) <- get
  let (h, st0) = newUE (ue e) st
  set (st0, (g, atom { atomChanWrite = atomChanWrite atom
                                    ++ [(cin, h, d)] }))

-- | Default delay specialization of the above.
writeChannel :: Expr a => ChanInput -> E a -> Atom ()
writeChannel = writeChannelWithDelay DelayDefault

-- | Read a message from a typed channel. This action returns an expression
-- representing the value of the last message written (or the initial content).
-- In the course of reading a message, it is consumed.
readChannel :: ChanOutput -> Atom (E a)
readChannel cout = do
  (st, (g, atom)) <- get
  set (st, (g, atom { atomChanRead = atomChanRead atom ++ [cout] }))
  return . VRef . V . chanVar $ cout

-- | Check if the channel contains a message.
fullChannel :: ChanOutput -> E Bool
fullChannel = VRef . V . readyVar


-- Helpers -------------------------------------------------------------------

-- | State struct name prefix for channel variables
channelPrefix :: String
channelPrefix = "__channel_"

-- | Construct a channel variable which, in the C code generation case, is a
-- stand-in for part of the global state sructure (the part storing the channel
-- content).
chanVar :: HasChan b => b -> UV
chanVar c = UVChannel (chanID c) (chanName c) (chanType c)

-- | Not exported. Use condChannel instead to condition execution of an Atom
-- on the readiness of a channel.
readyVar :: HasChan b => b -> UV
readyVar c = UVChannelReady (chanID c) (chanName c)
