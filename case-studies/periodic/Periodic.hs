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

import Language.LIMA
import Language.Sally


-- Example 1 -------------------------------------------------------------------

-- | Simple periodic atom.
ex1 :: Atom ()
ex1 = atom "ex1" $ do
  x <- int64 "x" 0
  incr x
  assert "x is bounded" (value x <. 10)  -- obviously untrue

-- | Clocked version of ex1
ex2 :: Atom ()
ex2 = clocked 5 0 ex1  -- assertion is still not ture

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

  atom "alice" $ clocked 2 1 $ do
    x <- int8 "x" 0
    writeChannel acin true
    decr x

    atom "alice_rx" $ do
      cond $ fullChannel bcout
      _ <- readChannel bcout
      incr x

    assert "alice's x bounded" ((value x <=. 3) &&. (value x >=. (-3)))

  atom "bob" $ clocked 2 0 $ do
    x <- int8 "x" 0
    writeChannel bcin true
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

  clocked 2 0 $ atom "atomX" $ do
    incr x

  clocked 5 0 $ atom "atomY" $ do
    incr y

  assert "y < 4" (value y <. 4)
