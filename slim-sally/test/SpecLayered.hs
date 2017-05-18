-- |
-- Module      :  SpecLayered
-- Copyright   :  Benjamin Jones 2017
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  very
--
-- This module exports some higher level networking architectures to be used
-- in Atom specifications.
--

module SpecLayered
  ( -- * switched architectures
    mkSWEther
    -- * bus architectures
  , mkStarBus
    -- * examples
  , atomWithSWEther
  , atomWithBus
  )
where

import Control.Monad (forM, forM_)
import Data.Int
import Text.Printf

import Language.SLIM hiding (compile)


-- Switched Architectures ------------------------------------------------------

typ :: Type
typ = Int64
type Typ = Int64

-- | Instantiate a switched, broadcasting ethernet network. The outputs are a
-- channel input/output for each node.
--
-- o--e--\ /--e--o
--     \  S  /
--      \/ \/
--      /\ /\
--     /  S  \
-- o--e--/ \--e--o
--
-- In the diagram, o's are nodes, e's are "endpoints" and S's are switches.
-- Furthermore, the nodes and endpoints on the right hand side are the same
-- as the nodes and endpoints on the left hand side. The diagram is just
-- unrolled for illustrative purposes.
--
-- TODO: The type of messages handled by the network is fixed to be 'typ'
-- defined above. This is due to a limitation with the channel types that needs
-- to be addressed.
--
mkSWEther :: Int   -- ^ number of nodes on each side of the network
          -> Int   -- ^ number of switches in the network
                   --   (# of redundant channels)
          -> Atom [(ChanInput, ChanOutput)]
          -- ^ n-(input, output) pairs to connect to nodes on left/right sides
mkSWEther n m = do
  let mkEndChans nm = unzip <$> mapM (flip channel typ) [nm ++ show i | i <- [0..n-1]]
  (nte_i, nte_o) <- mkEndChans "node_to_end_"
  (etn_i, etn_o) <- mkEndChans "end_to_node_"
  let res = zip nte_i etn_o  -- nte_o and etn_i are internal to the network

  -- generate the internal channels: [ [ (chan_into_net, chan_out_net) ] ]
  internalChans <- forM [0..m-1] $ \j -> do    -- loop over switches
                     forM [0..n-1] $ \i -> do  -- loop over endpoints
                       c1 <- channel (printf "e%d_to_sw%d" i j) typ
                       c2 <- channel (printf "sw%d_to_e%d" j i) typ
                       return (c1, c2)
  let getIncoming cs = map (snd . fst) cs ++ map (snd . snd) cs
  let getOutgoing cs = map (fst . fst) cs ++ map (fst . snd) cs

  -- generate the switches
  forM_ [0..m-1] $ \j -> do
    atom (printf "sw%d" j) $ do
      -- listen on each incoming chan and broadcast to all outgoing chans
      let myChans = internalChans !! j  -- :: [ (chan_into_net, chan_out_net) ]
      let myIncoming = getIncoming myChans
      let myOutgoing = getOutgoing myChans
      forM (zip myIncoming [0..]) $ \(cout, k) -> do
        atom (printf "handler%d" (k :: Int)) $ do
          cond $ fullChannel cout
          v <- readChannel cout
          mapM_ (flip writeChannel (v :: E Typ)) myOutgoing

  -- generate the endpoints
  forM_ [0..n-1] $ \i -> do
    let myChans = map (!! i) internalChans  -- :: [ (chan_into_net, chan_out_net) ]
    let myIncoming = getIncoming myChans
    let myOutgoing = getOutgoing myChans
    let myNodeCout = nte_o !! i
    atom ("endpoint_to_net" ++ show i) $ do
      -- listen on special endpoint channel and broadcast
      cond $ fullChannel myNodeCout
      v <- readChannel myNodeCout
      mapM_ (flip writeChannel (v :: E Typ)) myOutgoing  -- broadcast

    -- listen to all switches and write any receives to special node channel
    -- input
    forM_ [0..m-1] $ \j -> do
      let swOutput = myIncoming !! j
      atom ("endpoint_from_net" ++ show i) $ do
        cond $ fullChannel swOutput
        v <- readChannel swOutput
        writeChannel (etn_i !! j) (v :: E Typ)

  return res


-- Bus Architectures -----------------------------------------------------------

-- | Make a "star interconnect" bus for 'n' nodes.
--
--    o      o      o
--    |      |      |
--     \_____S_____/
--
-- In the diagram, o's are nodes and the S represents the star interconnect.
-- Each of the lines is a bidirectional channel, implemented as a pair of channels.
-- The user gets back only "half" of each of the bidirectional channels,
-- specifically the half that it can operate on (sending messsages to the bus,
-- receiving messages from bus).
--
-- The type of messages on the bus is fixed to be 'Typ' defined above. See
-- TODO above.
--
mkStarBus :: Int  -- ^ number of nodes on the bus
          -> Atom [(ChanInput, ChanOutput)]
          -- ^ for each node, a channel input/output pair for the node to
          -- communicate to the bus with
mkStarBus n = do
  -- make channels from nodes to star
  let mkChans nm = unzip <$> mapM (flip channel typ) [nm ++ show i | i <- [0..n-1]]
  (nts_i, nts_o) <- mkChans "inward"  -- "nts" = "node to star"

  -- make channels from star to nodes
  (stn_i, stn_o) <- mkChans "outward"  -- "stn" = "star to node"

  -- make the star node: listens to all incoming channels,
  -- on reciept it broadcasts to all outgoing channels.
  atom "star" $ do
    forM_ [0..n-1] $ \i ->
      let c = nts_o !! i in
      atom ("listen_inward_" ++ show i) $ do
        cond $ fullChannel c
        m' <- readChannel c
        -- send messages to all the other nodes attached to the bus
        mapM_ (flip writeChannel (m' :: E Typ)) [ stn_i !! j | j <- [0..n-1], i /= j ]

  -- return the channel end points that are relevant to the nodes
  return [(nts_i !! i, stn_o !! i) | i <- [0..n-1]]


-- Examples --------------------------------------------------------------------

-- | A simple system A --> B where the link inbetween is realized as a
-- redundant switched ethernet network.
atomWithSWEther :: Atom ()
atomWithSWEther = atom "atomLayered" $ do

  -- declare the switched ethernet fabric for 2 nodes with 2 internal switches
  chans <- mkSWEther 2 2
  let (nodeAToE, eToNodeA) = chans !! 0  :: (ChanInput, ChanOutput)
  let (_       , eToNodeB) = chans !! 1  :: (ChanInput, ChanOutput)

  -- node A
  atom "node_A" $ do
    msg <- int64 "msg" 0
    -- send a message
    atom "sender" $ do
      done <- bool "done" False
      cond $ not_ (value done)
      writeChannel nodeAToE (1 :: E Typ)
      done <== true

    atom "receiver" $ do
      -- store received messages
      cond $ fullChannel eToNodeA
      v <- readChannel eToNodeA
      msg <== v

  atom "node_B" $ do
    msg <- int64 "msg" 0
    -- store received messages
    cond $ fullChannel eToNodeB
    v <- readChannel eToNodeB
    msg <== v


-- | A simple system of three nodes connected to a bus.
atomWithBus :: Atom ()
atomWithBus = atom "atomBus" $ do
  -- some constants with type annotations so we don't have to repeat them
  let zero  = Const 0 :: E Int64
      one   = Const 1 :: E Int64
      two   = Const 2 :: E Int64
      three = Const 3 :: E Int64

  -- create the bus fabric, getting back channel endpoints that nodes can use
  [csA, csB, csC] <- mkStarBus 3

  -- Function that makes a node parametrized on 'per', a period (counted in
  -- terms of messages received), 'cin' a channel input to the bus, and 'cout'
  -- a channel outout from the bus. The node listens for messages from A B or
  -- C and increments counters accordingly. Every 'per' messages it receives
  -- it broadcasts it's own message to the bus.
  let mkNode ident per cin cout = atom ("node_" ++ show ident) $ do
        seenA <- int64 "seenA" 0
        seenB <- int64 "seenB" 0
        seenC <- int64 "seenC" 0
        counter <- int64 "counter" 0

        -- listener
        atom "listener" $ do
          cond $ fullChannel cout
          v <- readChannel cout
          -- TODO this is awkward to express
          seenA <== mux (v ==. one) (1 + value seenA) (value seenA)
          seenB <== mux (v ==. two) (1 + value seenB) (value seenB)
          seenC <== mux (v ==. three) (1 + value seenC) (value seenC)
          incr counter

        -- broadcaster
        atom "broadcaster" $ do
          cond $ (value counter) >. per
          counter <== zero
          writeChannel cin ident

  -- create nodes with different parameters using the mkNode function
  mkNode one   two   (fst csA) (snd csA)    -- node 1, period 2
  mkNode two   three (fst csB) (snd csB)    -- node 2, period 3
  mkNode three one   (fst csC) (snd csC)    -- node 3, period 1

  -- kickstart the system by putting a message on the bus as node C
  done <- bool "done" False
  cond $ not_ (value done)
  done <== true
  writeChannel (fst csC) three
