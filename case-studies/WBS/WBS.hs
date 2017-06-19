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

  -- Declare two lanes
  laneIns <- mapM mkLane [True, False]  -- high/low priority

  -- self loop to initialize button
  (initButtonIn, initButtonOut) <- channel "initButton" Bool
  atom "initTheButton" $ do
    done <- bool "done" False
    cond $ not_ (value done)
    writeChannel initButtonIn (Const True)
    done <== Const True

  -- (cin, cout) <- channel "button_chan" msgType
  clocked buttonPeriod buttonPhase . atom "button" $ do
    count <- var "count" zero      -- button's frame count
    bs <- bool "bs" False          -- button state
    cond $ fullChannel initButtonOut

    probe "boolbutton.count" (value count)
    bs <== mux (value bs ==. Const True) (Const False) (Const True)

    mapM_ (\(a, b, c) -> do
      writeChannel a (value bs) -- channel for command button input
      writeChannel b (value bs) -- channel for monitor button input
      writeChannel c (value count))  -- send 'count' to observer
      laneIns

    incr count

    writeChannel initButtonIn (Const True)  -- value doesn't matter

  printAllProbes


-- Command / Monitor "Lane" --------------------------------------------

pName :: Bool -> String -> String
pName b nm = nm ++ if b then "_high" else "_low"

mkLane :: Bool
       -> Atom (ChanInput, ChanInput, ChanInput)
mkLane pp = atom (pName pp "lane") $ do

  let probeP nm = probe (pName pp nm)

  (btcIn, btcOut) <- channel (pName True "btc") Bool  -- button to COM
  (btmIn, btmOut) <- channel (pName True "btm") Bool  -- button to MON
  (btoIn, btoOut) <- channel (pName True "bto") Bool  -- button to OBS

  -- com to mon state exchange
  (ctmIn, ctmOut) <- channel "ctm" Bool
  (ctoIn, ctoOut) <- channel "cto" Int64
  (mtoIn, mtoOut) <- channel "mto" Int64

  -- self loop to initialize button
  (initCOMIn, initCOMOut) <- channel "initCOM" Bool
  atom "initTheCOM" $ do
    done <- bool "done" False
    cond $ not_ (value done)
    writeChannel initCOMIn (Const True)
    done <== Const True

  -- COM node
  clocked procPeriod procPhase . atom "command" $ do
    bs         <- var "bs" False         -- observered button value
    prevbs     <- var "prevbs" False     -- previous button value
    framecount <- var "framecount" zero
    cautoMode  <- var "cautoMode" False
    cond $ fullChannel initCOMOut

    incr framecount
    prevbs <== value bs
    -- detect a rising edge in 'bs'
    cautoMode <== mux ((value bs ==. Const True) &&.
                       (value prevbs ==. Const False))
                      (not_ (value cautoMode))
                      (value cautoMode)
    writeChannel ctoIn (value framecount)  -- send 'framecount' to observer
    writeChannel ctmIn (value cautoMode)   -- send 'cautoMode' to MON

    writeChannel initCOMIn (Const True)     -- kick self
    probeP "command.autoMode" (value cautoMode)

    atom "wait_for_button_press" $ do
      cond $ fullChannel btcOut
      v <- readChannel btcOut
      bs <== v
      probeP "command.button_pressed" (value bs)

  -- self loop to initialize monitor
  (initMONIn, initMONOut) <- channel "initMON" Bool
  atom "initTheMON" $ do
    done <- bool "done" False
    cond $ not_ (value done)
    writeChannel initMONIn (Const True)
    done <== Const True

  -- MON node
  clocked procPeriod procPhase . atom "monitor" $ do
    framecount            <- var "count" zero
    bs                    <- var "bs"  False
    prevbs                <- var "prevbs" False
    mautoMode             <- var "mautoMode" False
    xSideAutoMode         <- var "autoMode" False
    agreementFailureCount <- var "agreementFailureCount" zero
    agreementFailure      <- var "agreementFailure" False
    cond $ fullChannel initMONOut

    incr framecount
    prevbs <== value bs
    -- detect rising edge
    mautoMode <== mux ((value bs ==. Const True) &&.
                       (value prevbs ==. Const False))
                      (not_ (value mautoMode))
                      (value mautoMode)
    writeChannel mtoIn (value framecount)  -- send 'framecount' to observer
    writeChannel initCOMIn (Const True)   -- kick self
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
      -- cond $ value mautoMode /=. value xSideAutoMode
      -- incr agreementFailureCount

    atom "mon_agreement_count" $ do
      cond $ value agreementFailureCount ==. Const three
      agreementFailure <== Const True


  -- | Internal Observer Node - run every tick and read values from the
  -- observer channels
  atom "observer" $ do
    bcount <- var "bcount" zero
    ccount <- var "ccount" zero
    mcount <- var "mcount" zero
    probeP "observer.bcount" (value bcount)
    probeP "observer.com_framecount" (value ccount)
    probeP "observer.mon_framecount" (value mcount)

    atom "wait_for_button_frame" $ do
     cond $ fullChannel btoOut
     v <- readChannel btoOut
     bcount <== v

    atom "wait_for_com_frame" $ do
     cond $ fullChannel ctoOut
     v <- readChannel ctoOut
     ccount <== v

    atom "wait_for_mon_frame" $ do
     cond $ fullChannel mtoOut
     v <- readChannel mtoOut
     mcount <== v

  -- return input channels for use by the button
  return (btcIn, btmIn, btoIn)


-- Utility Stuff -------------------------------------------------------

zero, one, three :: Int64
zero = 0
one = 1
three = 3

printAllProbes :: Atom ()
printAllProbes = mapM_ printProbe =<< probes
