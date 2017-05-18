-- |
-- Module      :  SpecClocked
-- Copyright   :  Benjamin Jones 2017
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Library functions for taking 'Atom's and clocking them.
--

module SpecClocked where


import Data.Int

import Language.SLIM


-- | Same as 'writeChannel' but adds a specified delay.
writeChannelWithDelay :: Int64
                      -> ChanInput
                      -> E a
                      -> Atom ()
writeChannelWithDelay = error "not implemented"

-- | Takes a node and conditions its execution on a regular clock with given
-- start (relative to the global clock) and period.
clocked :: Int64  -- ^ start time
        -> Int64  -- ^ period
        -> Atom a  -- ^ node to clock
        -> Atom a
clocked start per node = do
    init_out <- mkInit
    (ki, ko) <- channel "kick" Bool
    atom "clocked" $ do
      cond $ (fullChannel ko) ||. (fullChannel init_out)
      _ <- readChannel ko
      _ <- readChannel init_out
      writeChannelWithDelay per ki (Const True)
      node
  where
    mkInit :: Atom ChanOutput
    mkInit = do
      done <- bool "done" False
      (ii, io) <- channel "init" Bool

      atom "init" $ do
        cond $ not_ (value done)
        writeChannelWithDelay start ii (Const True)
        done <== Const True

      return io


-- Examples --------------------------------------------------------------------

-- | Example system where node A sends messages to node B. A has a period of 5
-- and B has a peroid of 1.
exampleClocked :: Atom ()
exampleClocked = atom "exampleClocked" $ do

  (cin, cout) <- channel "aToB" Bool

  -- declare clocked version of node A
  clocked 0 5 $ atom "node_A" $ do
    writeChannel cin true

  clocked 0 1 $ atom "node_B" $ do
    counter  <- int64 "counter" 0
    messages <- int64 "messages" 0

    atom "listener" $ do
      cond $ fullChannel cout
      _ <- readChannel cout  -- clear the channel
      incr messages

    incr counter
