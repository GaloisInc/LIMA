-- |
-- Module      :  SMP
-- Copyright   :  Benjamin Jones 2017
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple Message Passing example in LIMA. In this system, node A sends a
-- message to node B. Node B stores the received message and passes it on to
-- node C.
--
module SMP (module SMP) where

import Data.Int

import Language.LIMA
import Language.Sally

zero :: E Int64
zero = Const 0
zero' :: Int64
zero' = 0

ans :: E Int64
ans = Const 42

smp :: Atom ()
smp = atom "smp" $ do

  (tx0, rx0) <- channel "atob" Int64
  (tx1, rx1) <- channel "btoc" Int64

  atom "nodeA" $ do
    done <- var "done" False
    -- no cond
    done <== true
    writeChannel tx0 ans
    -- an intentional counterexample
    assert "nodeA not done" (not_ (value done))

  atom "nodeB" $ do
    done <- var "done" False
    store <- var "store" zero'
    cond $ fullChannel rx0
    v <- readChannel rx0
    store <== v
    done <== true
    writeChannel tx1 v
    -- an intentional counterexample
    assert "nodeB not done" (not_ (value done))

  atom "nodeC" $ do
    done <- var "done" False
    store <- var "store" zero'
    cond $ fullChannel rx1
    v <- readChannel rx1
    store <== v
    done <== true

    -- this property holds
    assert "nodeC done" (imply (value done) (value store ==. ans))

