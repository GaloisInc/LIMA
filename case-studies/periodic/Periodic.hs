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
module Periodic
  ( ex1
  , ex2
  , ex3
  )
where

import Language.SLIM
import Language.Sally


-- Example 1 -------------------------------------------------------------------

-- | Simple periodic atom.
ex1 :: Atom ()
ex1 = atom "ex1" $ do
  b <- bool "b" False
  b <== true

-- | Clocked version of ex1
ex2 :: Atom ()
ex2 = clocked 5 0 ex1

-- | Example with two subatoms that communicate
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

  atom "bob" $ clocked 2 0 $ do
    x <- int8 "x" 0
    writeChannel bcin true
    decr x

    atom "bob_rx" $ do
      cond $ fullChannel acout
      _ <- readChannel acout
      incr x
