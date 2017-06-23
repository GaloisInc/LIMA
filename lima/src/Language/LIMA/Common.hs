-- |
-- Module: Common
-- Description: Common functions.
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Common LIMA functions

module Language.LIMA.Common
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
    -- * Common rewrites
    -- , rewritePeriodPhase
  )
  where

import Data.Int
import Data.Maybe (mapMaybe)
import Data.Word
import MonadLib     -- re-exports Control.Monad
import Text.Printf

import Language.LIMA.Language

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
-- (exact) phase and period. Note that, strictly speaking, the given phase can
-- be larger than the period. This has the effect of delaying execution some
-- number of extra periods.
clocked :: Word64  -- ^ period
        -> Word64  -- ^ phase
        -> Atom a  -- ^ node to clock
        -> Atom a
clocked per pha node = do
  b <- cctxPeriodicity <$> ask
  if b
    then do
      nm0 <- getName                  -- get the outer atom's name
      let nm1 = getCompiledName node  -- get the inner atom's name
          nm = nm0 ++ nm1
      init_out <- mkInit nm
      (ki, ko) <- channel (nm ++ "_kick_channel") Bool
      atom (nm ++ "_clocked") $ do
        cond $ fullChannel ko ||. fullChannel init_out
        _ <- readChannel ko
        _ <- readChannel init_out
        writeChannelWithDelay (DelayTicks per) ki (Const True)
        atom (nm ++ "_node") node
    else period (fromIntegral per) (phase (fromIntegral pha) node)
  where
    mkInit :: Name -> Atom ChanOutput
    mkInit nm = do
      done <- bool (nm ++ "_init_done") False
      (ii, io) <- channel (nm ++ "_init_channel") Bool

      atom (nm ++ "_init") $ do
        cond $ not_ (value done)
        writeChannelWithDelay (DelayTicks pha) ii (Const True)
        done <== Const True

      return io


-- Switched Architectures ------------------------------------------------------

typ :: Type
typ = Int64
type Typ = Int64

typDef :: Typ
typDef = 0

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
  let bar i = [ l | l <- [0..n-1], l /= i ] :: [Int]
      rn    = [0..n-1] :: [Int]
      rm    = [0..m-1] :: [Int]
  let mkEndChans nm = unzip <$> mapM (`channel` typ) [nm ++ show i | i <- rn]
  (nte_i, nte_o) <- mkEndChans "node_to_end_"
  (etn_i, etn_o) <- mkEndChans "end_to_node_"
  let res = zip nte_i etn_o  -- nte_o and etn_i are internal to the network

  -- generate the internal channels: [ [ (in_k_j, [out_1, ...]) ] ]
  -- where in_k_j goes from endpoint j to switch k and out_1 .. out_{n-1} go
  -- from switch k to the other endpoints (but not the j-th.
  internalChans <-
    forM rm $ \k ->       -- loop over switches
      forM rn $ \j -> do  -- loop over endpoints
        in_k_j <- channel (printf "in_s%d_e%d" k j) typ
        let mkOChan i = do c <- channel (printf "out_s%v_e%v_e%v" k j i) typ
                           return (i,c)
        outs <- mapM mkOChan (bar j)
        return (in_k_j, outs)

  -- generate the switches:
  -- each one listes on each incoming chan and broadcast to all outgoing chans
  forM_ rm $ \k ->
    atom (printf "sw%v" k) $ do
      let myChans = internalChans !! k  -- :: [ (in_k_j, outs) ]_j
      forM_ rn $ \j -> do
        let (myIn, myOuts) = myChans !! j
        atom (printf "handler_%v_%v" k j) $ do
          cond $ fullChannel (snd myIn)
          v <- readChannel (snd myIn)
          mapM_ ((`writeChannel` (v :: E Typ)) . fst . snd) myOuts

  -- generate the endpoints
  forM_ rn $ \j -> do
    let myOuts = map (fst . (!! j)) internalChans
    let myNodeCout = nte_o !! j
    atom ("endpoint_to_net" ++ show j) $ do
      -- listen on special endpoint channel and broadcast
      cond $ fullChannel myNodeCout
      v <- readChannel myNodeCout
      mapM_ ((`writeChannel` (v :: E Typ)) . fst) myOuts  -- broadcast

    -- listen to all switches and write any receives to special node channel
    -- input
    -- Note: This has to buffered somehow
    buffer <- var (printf "buffer_%v" j) typDef
    ready  <- bool (printf "ready_%v" j) False
    let myIns = concatMap (mapMaybe (\(_, allouts) -> lookup j allouts))
                          internalChans
    forM_ (zip [0..] myIns) $ \(l, cl) ->
      atom (printf "endpoint_from_net_f%v_to_e%v" (l :: Int) j) $ do
        cond $ fullChannel (snd cl)
        v <- readChannel (snd cl)
        -- writeChannel (etn_i !! j) (v :: E Typ)
        buffer <== v
        ready  <== true

    -- write out the buffer
    atom (printf "endpoint_from_net_writer_e%v" j) $ do
      cond (value ready)
      writeChannel (etn_i !! j) (value buffer)
      ready <== false

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
  let mkChans nm = unzip <$> mapM (`channel` typ) [nm ++ show i | i <- [0..n-1]]
  (nts_i, nts_o) <- mkChans "inward"  -- "nts" = "node to star"

  -- make channels from star to nodes
  (stn_i, stn_o) <- mkChans "outward"  -- "stn" = "star to node"

  -- make the star node: listens to all incoming channels,
  -- on reciept it broadcasts to all outgoing channels.
  atom "star" $
    forM_ [0..n-1] $ \i -> do
      -- buffer writes to stn_i
      buffer <- var (printf "buffer_%v" i) typDef
      ready  <- bool (printf "ready_%v" i) False
      let c = nts_o !! i
      -- listen for incoming from other notes than the i-th
      forM_ [ j | j <- [0..n-1], i /= j ] $ \j ->
        atom (printf "listen_inward_%v_%v" i j) $ do
          cond $ fullChannel c
          m' <- readChannel c
          -- buffer messages to all the other nodes attached to the bus
          buffer <== m'
          ready  <== true
      -- write out the buffers when they're ready
      atom (printf "writer_%v" i) $ do
        cond (value ready)
        writeChannel (stn_i !! i) (value buffer)
        ready <== false

  -- return the channel end points that are relevant to the nodes
  return [(nts_i !! i, stn_o !! i) | i <- [0..n-1]]
