{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int
import qualified Data.Map.Strict as Map
import System.FilePath.Posix

import Language.LIMA hiding (compile)
import Language.Sally

testDir :: FilePath
testDir = "test"

type MsgType = Int64

msgType :: Type
msgType = Int64  -- Atom 'Type' value

-- Test Atoms -----------------------------------------------------------


-- | 'x' starts at 0, increases each tick up to 10
--   Property: G(atom1!x >= 0)
atom1 :: Atom ()
atom1 = atom "atom1" $ do
  x <- int8  "x" 0
  cond $ value x <. 10
  x <== value x + 1


-- | Two Atoms communicate through a flag
--   Property: G(atom2!alice!a => atom2!flag)
atom2 :: Atom()
atom2 = atom "atom2" $ do
  f <- bool "flag" False

  atom "alice" $ do
    a <- bool "a" False
    cond (value f)
    a <== true

  atom "bob" $ do
    f <== true


-- | Two Atoms communicate through a *channel*
--   Property: G((/= atom3!bob!msg -1) => atom3!alice!done)
--             F((/= atom3!bob!msg -1))
atom3 :: Atom()
atom3 = atom "atom3" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue = -1 :: MsgType
    goodMsgValue    = 1  :: MsgType

  (cin, cout) <- channel "aTob" msgType

  atom "alice" $ do
    done <- bool "done" False
    cond $ not_ (value done)
    writeChannel cin (Const goodMsgValue)
    done <== true

  atom "bob" $ do
    msg <- int64 "msg" missingMsgValue
    cond $ fullChannel cout
    v <- readChannel cout
    msg <== v

-- | A minimal version of atom3, where two agents commicate one message over a
-- channel. This version has no "done" flags.
--   Property: G((/= atom3min!bob!msg -1) => (= atom3min!__t 1))
--             F((= atom3min!__t 1))
atom3min :: Atom()
atom3min = atom "atom3" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1

  (cin, cout) <- channel "aTob" msgType

  atom "alice" $ do
    writeChannel cin (Const goodMsgValue)

  atom "bob" $ do
    msg <- int64 "msg" missingMsgValue
    cond $ fullChannel cout
    v <- readChannel cout
    msg <== v

-- | A periodic version of atom3, where two agents commicate one message over a
-- channel.
atom3Per :: Atom()
atom3Per = atom "atom3Per" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1
    nodeBPeriod     = 3

  (cin, cout) <- channel "aTob" msgType

  atom "alice" $ do
    done <- var "done" False
    cond $ not_ (value done)
    writeChannel cin (Const goodMsgValue)
    done <== Const True

  period nodeBPeriod . atom "bob" $ do
    msg <- int64 "msg" missingMsgValue
    cond $ fullChannel cout
    v <- readChannel cout
    msg <== v

-- | Three Atoms communicate through two channels
--
--   Property: node C is done implies that node C's 'msg' variable equals 1
--   ('goodMsgValue'). Futhermore, node C is done implies that the global time
--   is equal to 2.
--
--   (=> A4!atom4!nodeC!done (and (= A4!atom4!nodeC!msg 1)
--                                (= A4!__t 2)))
atom4 :: Atom()
atom4 = atom "atom4" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1

  (cinA2B, coutA2B) <- channel "a2b" msgType
  (cinB2C, coutB2C) <- channel "b2c" msgType

  atom "nodeA" $ do
    done <- bool "done" False
    writeChannel cinA2B (Const goodMsgValue)
    done <== Const True

  atom "nodeB" $ do
    done <- bool "done" False
    msg <- int64 "msg" missingMsgValue
    cond (fullChannel coutA2B)
    v <- readChannel coutA2B
    writeChannel cinB2C v
    msg <== v
    done <== Const True

  atom "nodeC" $ do
    done <- bool "done" False
    msg <- int64 "msg" missingMsgValue
    cond (fullChannel coutB2C)
    v <- readChannel coutB2C
    msg <== v
    done <== Const True

-- | Atom setting and using a timer based on the time at which it
--   received a message.
atom5 :: Atom()
atom5 = atom "atom5" $ do

  let
    -- | Special message values indicating "no message present", and "correct
    -- (intended) message"
    missingMsgValue, goodMsgValue  :: MsgType
    missingMsgValue = -1
    goodMsgValue    = 1

  (cin, cout) <- channel "chan" msgType

  atom "alice" $ do
    done <- bool "done" False
    writeChannel cin (Const goodMsgValue)
    done <== Const True

  atom "bob" $ do

    rxTime <- word64 "rxTime" 0

    atom "recMsg"  $ do
      msg <- int64 "msg" missingMsgValue
      cond $ fullChannel cout
      m <- readChannel cout
      msg <== m
      rxTime <== clock

    atom "timerDone" $ do
      local <- bool "local" False
      cond (value rxTime + 1000 >. clock)
      local <== Const True

-- | An atom for testing the difference between the regular guard 'cond' and
-- the non-inheriting version 'cond\''.
atom6 :: Atom ()
atom6 = atom "atom6" $ do
  a <- bool "a" True
  b <- bool "b" True

  -- take at least one transition
  cond  (value a)  -- subnode inherits
  cond' (value b)  -- subnode ignores
  a <== false
  b <== false

  -- sub_node does not inherit guard on 'a'
  atom "subnode" $ do
    c <- bool "c" True
    cond (value c)
    c <== false

-- Examples of Layerd Atoms --------------------------------------------------

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
          cond $ value counter >. per
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


-- Configurations --------------------------------------------------------------

-- | Default config for these specs
defSpecCfg :: TrConfig
defSpecCfg = defaultCfg { cfgDebug = True }

-- | Example of a hybrid fault model configuration.
hybridCfg :: TrConfig
hybridCfg = defSpecCfg { cfgMFA = HybridFaults ws 0 }
  where ws = Map.fromList [ (NonFaulty, 0), (ManifestFaulty, 1), (SymmetricFaulty, 2)
                          , (ByzantineFaulty, 3)
                          ]

-- | Example of a fixed fault mapping (specific to 'atom2' above).
fixedCfg :: TrConfig
fixedCfg = defSpecCfg { cfgMFA = FixedFaults mp }
  where mp = Map.fromList [ ("A2b!atom2!alice", NonFaulty)
                          , ("A2b!atom2!bob",   ByzantineFaulty)
                          ]

-- Main -----------------------------------------------------------------

putHeader :: IO ()
putHeader = putStrLn (replicate 72 '-')

testCompile :: (String, Atom (), TrConfig, String) -> IO ()
testCompile (nm, spec, cfg, q) = do
  let fname = testDir </> nm ++ ".mcmt"
  compileToSally nm cfg fname spec (Just q)
  putStrLn ("compiled " ++ fname)

-- | List of (Name, Atom, Query) to translate and print
suite :: [(String, Atom (), TrConfig, String)]
suite =
  [ ("A1", atom1, hybridCfg,
        "(query A1_transition_system (=> A1_assumptions (<= 0 A1!atom1!x)))")
  , ("A1b", atom1, defSpecCfg,
        "(query A1b_transition_system (=> A1b_assumptions (<= 0 A1b!atom1!x)))")
  , ("A2", atom2, hybridCfg,
        "(query A2_transition_system (=> A2_assumptions (=> A2!atom2!alice!a A2!atom2!flag)))")
  , ("A2b", atom2, fixedCfg,
        "(query A2b_transition_system (=> A2b_assumptions (=> A2b!atom2!alice!a A2b!atom2!flag)))")
  , ("A3", atom3, hybridCfg,
        unlines [ "(query A3_transition_system"
                , "    (=> (not (= A3!atom3!bob!msg (-1))) A3!atom3!alice!done))"])
    -- different config from A3
  , ("A3b", atom3, defSpecCfg,
        unlines [ "(query A3b_transition_system"
                , "    (=> (not (= A3b!atom3!bob!msg (-1))) A3b!atom3!alice!done))"])
    -- fewer state vars & different property
  , ("A3min", atom3min, defSpecCfg,
        unlines [ "(query A3min_transition_system"
                , "    (=> (not (= A3min!atom3!bob!msg (-1))) (>= A3min!__t 1)))"])
    -- receiver has a long period
  , ("A3per", atom3Per, defSpecCfg,
        unlines [ "(query A3per_transition_system"
                , "  (<= 0 A3per!__t))"
                , ""
                , "(query A3per_transition_system"
                , "    (=> (not (= A3per!atom3Per!bob!msg (-1))) (>= A3per!__t 1)))"])
  , ("A4", atom4, defSpecCfg,
        unlines [ "(query A4_transition_system"
                , "    (=> A4!atom4!nodeC!done (= A4!atom4!nodeC!msg 1)))"
                , "\n\n"
                , "(query A4_transition_system"
                , "    (=> A4!atom4!nodeC!done (= A4!__t 2)))"])
  , ("A5", atom5, defSpecCfg, "")
  , ("A6", atom5, defSpecCfg, "")
  , ("ASWEther", atomWithSWEther, defSpecCfg, "")
  , ("ABus", atomWithBus, defSpecCfg, "")
  ]

main :: IO ()
main = mapM_ testCompile suite
