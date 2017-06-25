{-# LANGUAGE OverloadedStrings #-}

module WBS (wbs) where

import Data.Int
import Data.Word
import qualified Data.Map.Strict as Map
import System.FilePath.Posix

import Language.LIMA
import Language.LIMA.C
import Language.Sally


-- Constants -------------------------------------------------------------------

testDir :: FilePath
testDir = "test"

type MsgType = Int64

msgType :: Type
msgType = Int64  -- Atom 'Type' value

-- Button press processes period
buttonPeriod :: Word64
buttonPeriod = 10
buttonPhase :: Word64
buttonPhase = 0

-- COM and MON period
procPeriod :: Word64
procPeriod = 2
procPhase :: Word64
procPhase = 0


-- Single Channel Wheel Brake Example ------------------------------------------

wbs :: Atom ()
wbs = atom "wbs" $ do

  -- cross channel comms
  (mhtomlIn, mhtomlOut) <- channel "mon_high_to_low" Bool
  (mltomhIn, mltomhOut) <- channel "mon_low_to_high" Bool

  -- Declare two lanes
  laneIns <- mapM mkLane [ (True, mhtomlIn, mltomhOut)
                         , (False, mltomhIn, mhtomlOut)
                         ]  -- high/low priority

  clocked buttonPeriod buttonPhase . atom "button" $ do
    count <- var "count" zero      -- button's frame count
    bs <- bool "bs" False          -- button state

    probe "boolbutton.count" (value count)
    bs <== mux (value bs ==. Const True) (Const False) (Const True)

    mapM_ (\(a, b, c) -> do
      writeChannel a (value bs) -- channel for command button input
      writeChannel b (value bs) -- channel for monitor button input
      writeChannel c (value count))  -- send 'count' to observer
      laneIns

    incr count

  printAllProbes


-- Command / Monitor "Lane" --------------------------------------------

-- | A lane consists of a command node, a monitor node, and an observer.
--
--   Input is a triple of: flag to indicate "high" lane or "low" lane, chan
--   input to write control messages on for the other mon, and chan output to
--   listen for control messaages on from the other mon.
--
--   Return includes (in order) a chan input that goes to the COM (Bool),
--   one that goes to the MON (Bool), and one that goes to the observer
--   (Int64).
mkLane :: (Bool, ChanInput, ChanOutput)
       -> Atom (ChanInput, ChanInput, ChanInput)
mkLane (pp, mtmIn, mtmOut) = atom (pName pp "lane") $ do

  let probeP nm = probe (pName pp nm)

  (btcIn, btcOut) <- channel (pName pp "btc") Bool   -- button to COM
  (btmIn, btmOut) <- channel (pName pp "btm") Bool   -- button to MON
  (btoIn, btoOut) <- channel (pName pp "bto") Int64  -- button to OBS

  -- com to mon and observer state exchange
  (ctmIn, ctmOut) <- channel (pName pp "ctm") Bool   -- send state
  (ctoIn, ctoOut) <- channel (pName pp "cto") Int64  -- send frame count
  (mtoIn, mtoOut) <- channel (pName pp "mto") Int64  -- send frame count

  -- COM node
  clocked procPeriod procPhase . atom (pName pp "command") $ do
    bs         <- var "bs" False         -- observered button value
    prevbs     <- var "prevbs" False     -- previous button value
    framecount <- var "framecount" zero
    cautoMode  <- var "cautoMode" False

    incr framecount
    prevbs <== value bs
    -- detect a rising edge in 'bs'
    cautoMode <== mux ((value bs ==. Const True) &&.
                       (value prevbs ==. Const False))
                      (not_ (value cautoMode))
                      (value cautoMode)
    writeChannel ctoIn (value framecount)  -- send 'framecount' to observer
    writeChannel ctmIn (value cautoMode)   -- send 'cautoMode' to MON

    probeP "command.autoMode" (value cautoMode)

    atom "wait_for_button_press" $ do
      cond $ fullChannel btcOut
      v <- readChannel btcOut
      bs <== v
      probeP "command.button_pressed" (value bs)

  -- MON node
  clocked procPeriod procPhase . atom (pName pp "monitor") $ do
    framecount            <- var "count" zero
    bs                    <- var "bs"  False
    prevbs                <- var "prevbs" False
    mautoMode             <- var "mautoMode" False
    xSideAutoMode         <- var "autoMode" False
    agreementFailureCount <- var "agreementFailureCount" zero
    agreementFailure      <- var "agreementFailure" False
    control               <- var "control" pp
    otherControl          <- var "otherControl" False

    incr framecount
    prevbs <== value bs
    -- detect rising edge
    mautoMode <== mux ((value bs ==. Const True) &&.
                       (value prevbs ==. Const False))
                      (not_ (value mautoMode))
                      (value mautoMode)
    writeChannel mtoIn (value framecount)  -- send 'framecount' to observer
    probeP "monitor.autoMode" (value mautoMode)
    probeP "monitor.agreementFailureCount" (value agreementFailureCount)
    probeP "monitor.agreementFailure" (value agreementFailure)

    atom "wait_for_button_press" $ do
      cond $ fullChannel btmOut
      v <- readChannel btmOut
      bs <== v
      probeP "monitor.button_pressed" (value bs)

    atom "wait_x_side_autoMode" $ do
      cond $ fullChannel ctmOut
      v <- readChannel ctmOut
      xSideAutoMode <== v
      probeP "monitor.XsideAutoMode" (value xSideAutoMode)

    atom "mon_agreement" $ do
      agreementFailureCount <==
        mux (value mautoMode /=. value xSideAutoMode)
            (Const one + value agreementFailureCount)
            (Const zero)
      assert (pName pp "my assert") (value agreementFailureCount <=. Const three)

    atom "mon_agreement_count" $ do
      cond $ value agreementFailureCount ==. Const three
      agreementFailure <== Const True
      control <== false
      writeChannel mtmIn (value control)

    -- read values of "control" from the other mon, indicating whether that
    -- mon thinks it is in control or not
    atom "mon_other_control" $ do
      cond $ fullChannel mtmOut
      v <- readChannel mtmOut
      otherControl <== v

  -- | Internal Observer Node - run every tick and read values from the
  -- observer channels
  atom (pName pp "observer") $ do
    bcount <- var "bcount" zero
    ccount <- var "ccount" zero
    mcount <- var "mcount" zero
    probeP "observer.bcount" (value bcount)
    probeP "observer.com_framecount" (value ccount)
    probeP "observer.mon_framecount" (value mcount)

    atom "wait_for_button_frame" $ do
     cond $ fullChannel btoOut
     _ <- readChannel btoOut
     incr bcount

    atom "wait_for_com_frame" $ do
     cond $ fullChannel ctoOut
     _ <- readChannel ctoOut
     incr ccount

    atom "wait_for_mon_frame" $ do
     cond $ fullChannel mtoOut
     _ <- readChannel mtoOut
     incr mcount

  -- return input channels for use by the button
  return (btcIn, btmIn, btoIn)


-- Utility Stuff -------------------------------------------------------

zero, one, three :: Int64
zero  = 0
one   = 1
three = 3

printAllProbes :: Atom ()
printAllProbes = mapM_ printProbe =<< probes

-- | Helper function to append "high" or "low" to names of various components
pName :: Bool -> String -> String
pName b nm = nm ++ if b then "_high" else "_low"
