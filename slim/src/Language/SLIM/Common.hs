-- |
-- Module: Common
-- Description: Common functions.
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Common SLIM functions

module Language.SLIM.Common
  (
  -- * Timers
    Timer
  , timer
  , startTimer
  , startTimerIf
  , timerDone
  -- * One Shots
  , oneShotRise
  , oneShotFall
  -- * Debouncing
  , debounce
  -- * Lookup Tables
  , lookupTable
  , linear
  -- * Hysteresis
  , hysteresis
  -- * Periodic clocks
  , clocked
    -- * switched architectures
  , mkSWEther
    -- * bus architectures
  , mkStarBus
  )
  where

import Control.Monad (forM, forM_)
import Data.Int
import Data.Word
import Text.Printf

import Language.SLIM.Language


-- | A Timer.
newtype Timer = Timer { timerVar :: V Word64 }

-- | Creates a new timer.
timer :: Name -> Atom Timer
timer name = do
  timer' <- word64 name 0
  return $ Timer timer'

-- | Starts a Timer.  A timer can be restarted at any time.
startTimer :: Timer    -- ^ Timer to start
           -> E Word64 -- ^ Number of clock ticks the timer shall run
           -> Atom ()
startTimer t = startTimerIf t true

-- | Conditionally start a Timer.
startTimerIf :: Timer    -- ^ Timer to start conditionally
             -> E Bool   -- ^ Condition for starting the timer
             -> E Word64 -- ^ Number of ticks the timer shall run
             -> Atom ()
startTimerIf t a time = timerVar t <== mux a (clock + time) (value (timerVar t))

-- | 'True' when a timer has completed. Note that this remains 'True' until
-- the timer is restarted.
timerDone :: Timer -> E Bool
timerDone t = value (timerVar t) <=. clock

-- | One-shot on a rising transition.
oneShotRise :: E Bool -> Atom (E Bool)
oneShotRise a = do
  last' <- bool "last" False
  last' <== a
  return $ a &&. not_ (value last')

-- | One-shot on a falling transition.
oneShotFall :: E Bool -> Atom (E Bool)
oneShotFall = oneShotRise . not_

-- | Debounces a boolean given an on and off time (ticks) and an initial state.
debounce :: Name          -- ^ Name of the resulting atom
         -> E Word64      -- ^ On time in ticks
         -> E Word64      -- ^ Off time in ticks
         -> Bool          -- ^ Initial value
         -> E Bool        -- ^ The boolean to debounce
         -> Atom (E Bool) -- ^ Resulting debounced boolean
debounce name onTime offTime init' a = atom name $ do
  lst    <- bool "last" init'
  out    <- bool "out"  init'
  timer' <- timer "timer"
  atom "on" $ do
    cond $ a &&. not_ (value lst)
    startTimer timer' onTime
    lst <== a
  atom "off" $ do
    cond $ not_ a &&. value lst
    startTimer timer' offTime
    lst <== a
  atom "set" $ do
    cond $ a ==. value lst
    cond $ timerDone timer'
    out <== value lst
  return $ value out

-- | 1-D lookup table.  @x@ values out of table range are clipped at end @y@
-- values.  Input table must be monotonically increasing in @x@.
lookupTable :: FloatingE a
            => [(E a, E a)] -- ^ (@x@, @y@) lookup table
            -> E a          -- ^ Input @x@ value
            -> E a          -- ^ Output @y@ value
lookupTable table x = mux (x >=. x1) y1 $ foldl f y0 table'
  where
  (_,  y0) = head table
  (x1, y1) = last table
  table' = zip (init table) (tail table)
  f a ((a0,b0),(a1,b1)) = mux (x >=. a0) interp a
    where
    slope = (b1 - b0) / (a1 - a0)
    interp = (x - a0) * slope + b0

-- | Linear extrapolation and interpolation on a line with 2 points.
-- The two @x@ points must be different to prevent a divide-by-zero.
linear :: FloatingE a
       => (E a, E a) -- ^ First point, (x1, y1)
       -> (E a, E a) -- ^ Second point, (x2, y2)
       -> E a        -- ^ Input @x@ value
       -> E a        -- ^ Interpolated/extrapolated @y@ value
linear (x1, y1) (x2, y2) a = slope * a + inter
  where
  slope = (y2 - y1) / (x2 - x1)
  inter = y1 - slope * x1

-- | Hysteresis returns 'True' when the input exceeds @max@ and 'False' when
-- the input is less than @min@.  The state is held when the input is between
-- @min@ and @max@.
hysteresis :: OrdE a
           => E a  -- ^ min
           -> E a  -- ^ max
           -> E a  -- ^ Input
           -> Atom (E Bool)
hysteresis a b u = do
  s <- bool "s" False
  s <== mux (u >. max') true (mux (u <. min') false (value s))
  return $ value s
  where
  min' = min_ a b
  max' = max_ a b


-- Periodic clock functions ----------------------------------------------------

-- | Takes a node and conditions its execution on a regular clock with given
-- start (relative to the global clock) and period.
clocked :: Word64  -- ^ start time
        -> Word64  -- ^ period
        -> Atom a  -- ^ node to clock
        -> Atom a
clocked start per node = do
    init_out <- mkInit
    (ki, ko) <- channel "kick" Bool
    atom "clocked" $ do
      cond $ fullChannel ko ||. fullChannel init_out
      _ <- readChannel ko
      _ <- readChannel init_out
      writeChannelWithDelay (DelayTicks per) ki (Const True)
      node
  where
    mkInit :: Atom ChanOutput
    mkInit = do
      done <- bool "done" False
      (ii, io) <- channel "init" Bool

      atom "init" $ do
        cond $ not_ (value done)
        writeChannelWithDelay (DelayTicks start) ii (Const True)
        done <== Const True

      return io


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
  internalChans <- forM [0..m-1] $ \j ->       -- loop over switches
                     forM [0..n-1] $ \i -> do  -- loop over endpoints
                       c1 <- channel (printf "e%d_to_sw%d" i j) typ
                       c2 <- channel (printf "sw%d_to_e%d" j i) typ
                       return (c1, c2)
  let getIncoming cs = map (snd . fst) cs ++ map (snd . snd) cs
  let getOutgoing cs = map (fst . fst) cs ++ map (fst . snd) cs

  -- generate the switches
  forM_ [0..m-1] $ \j ->
    atom (printf "sw%d" j) $ do
      -- listen on each incoming chan and broadcast to all outgoing chans
      let myChans = internalChans !! j  -- :: [ (chan_into_net, chan_out_net) ]
      let myIncoming = getIncoming myChans
      let myOutgoing = getOutgoing myChans
      forM (zip myIncoming [0..]) $ \(cout, k) ->
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
  atom "star" $
    forM_ [0..n-1] $ \i ->
      let c = nts_o !! i in
      atom ("listen_inward_" ++ show i) $ do
        cond $ fullChannel c
        m' <- readChannel c
        -- send messages to all the other nodes attached to the bus
        mapM_ (flip writeChannel (m' :: E Typ)) [ stn_i !! j | j <- [0..n-1], i /= j ]

  -- return the channel end points that are relevant to the nodes
  return [(nts_i !! i, stn_o !! i) | i <- [0..n-1]]

