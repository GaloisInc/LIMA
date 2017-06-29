-- |
-- Module      :  Periodic
-- Copyright   :  Benjamin Jones 2017
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Some example specifications of systems with different periodic behavior.
--
module Periodic (module Periodic) where

import Control.Monad (mapM_)
import Data.Int

import Language.LIMA
import Language.LIMA.C.Util (printProbe)
import Language.Sally


-- Example 1 -------------------------------------------------------------------

-- | Simple periodic atom.
ex1 :: Atom ()
ex1 = atom "ex1" $ do
  x <- int64 "x" 0
  incr x
  assert "x is bounded" (value x <. 3)  -- obviously untrue

-- | Clocked version of ex1
ex2 :: Atom ()
ex2 = atom "ex2" $ clocked 5 3 $ \res -> do
  x <- int64 "x" 0
  incr x
  mapM_ readChannel res  -- reset clock channel(s)
  assert "x is bounded" (value x <. 3)  -- obviously untrue

-- | Example with two subatoms that communicate. Each time that each of the
-- nodes executes, it decrements a counter and sends a message to the other
-- node. If it receives a message from the other node, it also increments the
-- same counter.
--
-- This system should satisfy the property that if the periods of the two
-- nodes are the same, then the counters cannot get large in absolute value.
ex3 :: Atom ()
ex3 = atom "ex3" $ do
  (acin, acout) <- channel "ach" Bool
  (bcin, bcout) <- channel "bch" Bool

  atom "alice" $ clocked 2 1 $ \res -> do
    x <- int8 "x" 0
    writeChannel acin true
    mapM_ readChannel res  -- reset clock channel(s)
    decr x

    atom "alice_rx" $ do
      cond $ fullChannel bcout
      _ <- readChannel bcout
      incr x

    assert "alice's x bounded" ((value x <=. 3) &&. (value x >=. (-3)))

  atom "bob" $ clocked 2 0 $ \res -> do
    x <- int8 "x" 0
    writeChannel bcin true
    mapM_ readChannel res  -- reset clock channel(s)
    decr x

    atom "bob_rx" $ do
      cond $ fullChannel acout
      _ <- readChannel acout
      incr x

    assert "bob's x bounded" ((value x <=. 3) &&. (value x >=. (-3)))

ex4 :: Atom ()
ex4 = atom "ex4" $ do
  x <- int64 "x" 0
  y <- int64 "y" 0

  clocked 2 0 $ \res -> atom "atomX" $ do
    incr x
    decr y
    probe "x + y" (value x + value y)
    mapM_ readChannel res  -- reset clock channel(s)

  clocked 5 3 $ \res -> atom "atomY" $ do
    incr y
    probe "y" (value y)
    mapM_ readChannel res  -- reset clock channel(s)

  assert "y <= 0" (value y <=. 0)  -- valid
  mapM_ printProbe =<< probes


-- | Example illustrating "kickstart" mechanism for period execution. The
-- library function 'clocked' in "Language.LIMA.Common" generalizes this
-- pattern.
ex5 :: Atom ()
ex5 = atom "ex5" $ do

  let ex5Phase = 3
  let ex5Period = 5

  (ii, io) <- channel "init_channel" Bool
  (ki, ko) <- channel "kick_channel" Bool
  (ni, no) <- channel "node_channel" Bool

  atom "kicker" $ do
    t <- var "test" (0 :: Int64)
    initChannel ii (CBool True) (DelayTicks ex5Phase)

    cond $ fullChannel io ||. fullChannel ko
    _ <- readChannel io
    writeChannelWithDelay (DelayTicks ex5Period) ki true
    writeChannelWithDelay (DelayTicks 0) ni true
    incr t

  atom "node" $ do
    x <- var "x" (0 :: Int64)
    cond $ fullChannel no
    _ <- readChannel no
    _ <- readChannel io
    incr x
    assert "x bounded" $ value x <. 5  -- smoke test

