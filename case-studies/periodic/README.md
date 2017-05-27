# atom-om1

[Atom](https://github.com/tomahawkins/atom) (Haskell eDSL) specification for a
concurrent [OM1](link) running on a shared memory distributed system.

# Usage

Compilation requires GHC, cabal, and a C99 compatible C compiler, and copies
of the GaloisInc `atom` and `atom-sally` repositories in a common directory.
Once the dependencies are met you can install and run the translators using
the `install.sh` script like so:

```
$ mkdir deps
$ cd deps
$ git clone https://github.com/GaloisInc/atom
$ git clone https://github.com/GaloisInc/atom-sally
$ cd ..

$ ATOMDIR=deps ./install.sh
```

This will build the C and Sally translators and translate the 'om1'
specification to code `om1.{c,h}` and model `om1.mcmt`, respectively.


# Simplified View of the specification

Here is a simplified view of the spec, without the probes and compiler
details. The `om1` declaration knits the source, relays, and receivers
together using a system of typed channels. The relays and receivers are
parameterized over an ID and set of input/output channels. Finally, the
`computeVote` function produces a completely unrolled expression for the
Boyer-Moore fast majority vote algorithm.

```haskell
-- Parameters ----------------------------------------------------------

-- Node clock periods (in ticks)
initPeriod     = 100
sourcePeriod   = 20
relayPeriod    = 7
recvPeriod     = 13
observerPeriod = 1

numRelays  = 3
numRecvs   = 3

relaySet = [0..numRelays-1]
recvSet  = [0..numRecvs-1]


-- OM(1) Spec ------------------------------------------------------------

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
  forM_ recvSet $ \ident ->
    recv ident [ snd ((r2rs !! i) !! ident) | i <- relaySet ]
         (votes !! ident)

  -- declare the observer
  observer


-- Source --------------------------------------------------------------

-- | Source node, a.k.a. "The General"
source :: [ChanInput]  -- ^ output channels to broadcast on
       -> Atom ()
source cs = period sourcePeriod
          . atom "source" $ do
  done <- bool "done" False

  -- activation condition
  cond $ not_ (value done)

  -- behavior
  done <== Const True
  forM_ cs $ \c -> do
    writeChannel c goodMsg


-- Relays --------------------------------------------------------------

-- | Relay node, a.k.a. a generic 0th round "Lieutenant"
relay :: Int          -- ^ relay id
      -> ChanOutput   -- ^ channel from source
      -> [ChanInput]  -- ^ channels to receivers
      -> Atom ()
relay ident inC outCs = period relayPeriod
                      . atom (tg "relay"  ident) $ do
  done <- bool "done" False
  msg  <- msgVar (tg "relay_msg" ident)

  -- activation condition
  cond $ isMissing msg  -- we haven't stored a value yet
  condChannel inC       -- there is a value available

  -- behavior
  msg  <== readChannel inC
  done <== Const True
  forM_ outCs $ \c -> do
    let m = readChannel inC :: E MsgType
    writeChannel c m


-- Receivers -----------------------------------------------------------

-- | Receiver node, a.k.a. a generic 1st round "Lieutenant"
recv :: Int           -- ^ receiver id
     -> [ChanOutput]  -- ^ channels from relays
     -> V MsgType
     -> Atom ()
recv ident inCs vote = period recvPeriod
                . atom (tg "recv" ident) $ do
  done <- bool "done" False
  buffer <- mapM msgVar [ tg (tg "buffer" ident) i | i <- relaySet ]

  -- declare multiple "pollers", one for each buffer location
  forM_ relaySet $ \i -> do
    atom (tg2 "recv_poll" ident i) $ do
      cond $ isMissing (buffer !! i)
      condChannel (inCs !! i)
      (buffer !! i) <== readChannel (inCs !! i)

  -- declare a voter
  atom (tg "recv_vote" ident) $ do
    cond $ all_ (not_ . isMissing) buffer
    vote <== computeVote (value <$> buffer)
    done <== Const True

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
        onTrue2       = c + (Const 1)
        onFalse1      = mux (c ==. Const 0) x y
        onFalse2      = mux (c ==. Const 0) (Const 1) (c - (Const 1))
        _             = c :: E Int64

-- | Synchronous observer node; current prints probe values to console at
-- phase 0. This node has no activation or behavior so its part in the model
-- is trivial.
observer :: Atom ()
observer = period observerPeriod
         . exactPhase 0
         . atom "observer" $ do
  ps <- probes
  mapM_ printProbe ps
```
