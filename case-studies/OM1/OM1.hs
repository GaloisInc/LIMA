-- |
-- Module      :  OM1
-- Copyright   :  Benjamin Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A specification for the distributed, fault tolerant system OM(1) written
-- using LIMA
--
module OM1
  ( om1
  )
where

import Control.Monad (forM, forM_)
import Data.Int

import Language.LIMA
import Language.LIMA.C (printProbe)
import Language.Sally


-- Parameters ----------------------------------------------------------

numRelays  = 3
numRecvs   = 3

relaySet = [0..numRelays-1]
recvSet  = [0..numRecvs-1]


-- OM(1) Spec ------------------------------------------------------------

-- | Top level rule
om1 :: Atom ()
om1 = do
  -- setup channels for communication between source, relays, and receivers
  s2rs <- mapM newChannel [ tg "s2r" i | i <- relaySet ]
  r2rs <- mapM (mapM newChannel) [ [ tg2 "r2r" i j | j <- recvSet ]
                                                   | i <- relaySet ]
  votes <- mapM msgVar [ tg "vote" j | j <- recvSet ]

  -- declare source node
  source (map fst s2rs)

  -- declare relay nodes
  forM_ relaySet $ \ident ->
    relay ident (snd (s2rs !! ident))
                (map fst (r2rs !! ident))

  -- declare receiver nodes
  dones <- forM recvSet $ \ident ->
    recv ident [ snd ((r2rs !! i) !! ident) | i <- relaySet ] (votes !! ident)

  assert "agreement" $ imply (and_ (map value dones)) (all_ (\(v,w) -> value v ==. value w)
                                                [ (v,w) | v <- votes, w <- votes ])
  assert "validity"  $ imply (and_ (map value dones)) (all_ (\v -> value v ==. goodMsg) votes)

  observer

-- Source --------------------------------------------------------------

-- | Source node, a.k.a. "The General"
source :: [ChanInput]  -- ^ output channels to broadcast on
       -> Atom ()
source cs = atom "source" $ do
  done <- bool "done" False

  -- activation condition
  cond $ not_ (value done)

  -- behavior
  done <== Const True
  forM_ cs $ \c -> writeChannel c goodMsg


-- Relays --------------------------------------------------------------

-- | Relay node, a.k.a. a generic 0th round "Lieutenant"
relay :: Int          -- ^ relay id
      -> ChanOutput   -- ^ channel from source
      -> [ChanInput]  -- ^ channels to receivers
      -> Atom ()
relay ident inC outCs = atom (tg "relay"  ident) $ do
  done <- bool "done" False
  msg  <- msgVar (tg "relay_msg" ident)

  -- activation condition:
  --   we haven't stored a value yet and there is a message waiting
  --   on the channel 'inC'
  cond $ isMissing msg &&. fullChannel inC

  -- behavior
  m <- readChannel inC :: Atom (E MsgType)
  msg  <== m
  done <== Const True
  forM_ outCs $ \c -> writeChannel c m


-- Receivers -----------------------------------------------------------

-- | Receiver node, a.k.a. a generic 1st round "Lieutenant"
recv :: Int           -- ^ receiver id
     -> [ChanOutput]  -- ^ channels from relays
     -> V MsgType
     -> Atom (V Bool)
recv ident inCs vote = atom (tg "recv" ident) $ do
  done <- bool "done" False
  buffer <- mapM msgVar [ tg (tg "buffer" ident) i | i <- relaySet ]

  -- declare multiple "pollers", one for each buffer location
  forM_ relaySet $ \i ->
    atom (tg2 "recv_poll" ident i) $ do
      cond $ isMissing (buffer !! i) &&. fullChannel (inCs !! i)
      b' <- readChannel (inCs !! i)
      (buffer !! i) <== b'

  -- declare a voter
  atom (tg "recv_vote" ident) $ do
    cond $ all_ (not_ . isMissing) buffer
    vote <== computeVote (value <$> buffer)
    done <== Const True

  return done


-- | Boyer-Moore Fast Majority Vote
computeVote :: [E MsgType] -> E MsgType
computeVote = fst . foldr iter (missingMsgValueE, Const 0)
  where
    iter x (y, c) = ( mux (x ==. y) onTrue1 onFalse1
                    , mux (x ==. y) onTrue2 onFalse2)
      where
        -- rules:
        --   if x ==. y, then     (y, c+1)
        --   else if c == 0, then (x, 1)
        --   else                 (y, c-1)
        onTrue1       = y
        onTrue2       = c + Const 1
        onFalse1      = mux (c ==. Const 0) x y
        onFalse2      = mux (c ==. Const 0) (Const 1) (c - Const 1)
        _             = c :: E Int64

-- | Synchronous observer node; current prints probe values to console at
-- phase 0. This node has no activation or behavior so its part in the model
-- is trivial.
observer :: Atom ()
observer = atom "observer" $ do
  ps <- probes
  mapM_ printProbe ps


-- Helper functions and definitions for Channels and Messages ----------

type MsgType = Int64
msgType = Int64

-- | Specially designated intended message to be send in the absense of faults
goodMsg :: E MsgType
goodMsg = Const 0

-- | Special message type value indicating "no message present"
missingMsgValue :: MsgType
missingMsgValue = 0

missingMsgValueE :: E MsgType
missingMsgValueE = Const 0

isMissing :: V MsgType -> E Bool
isMissing = (==. missingMsgValueE) . value

-- | Declare a new channel with 'missingMsgValue' as its initial value
newChannel :: String -> Atom (ChanInput, ChanOutput)
newChannel = flip channel msgType

-- | Declare a variable of message type and add a probe for it to the
-- environment
msgVar :: Name -> Atom (V MsgType)
msgVar nm = do
  v <- msgVar' nm
  probe nm (value v)
  return v

-- | Declare a message variable w/o adding a probe
msgVar' :: Name -> Atom (V MsgType)
msgVar' nm = int64 nm missingMsgValue

-- | Tag a name with an ID
tg :: Name -> Int -> Name
tg nm i = nm ++ "_" ++ show i

-- | Tag a name with a pair of IDs
tg2 :: Name -> Int -> Int -> Name
tg2 nm i j = nm ++ "_" ++ show i ++ "_" ++ show j
