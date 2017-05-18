{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Int
import qualified Data.Map.Strict as Map
import System.FilePath.Posix

import Language.SLIM hiding (compile)
import qualified Language.SLIM as A
import Language.Sally


-- Constants -------------------------------------------------------------------

testDir :: FilePath
testDir = "test"

type MsgType = Int64

msgType :: Type
msgType = Int64  -- Atom 'Type' value

-- Button press processes period
buttonPeriod :: Int
buttonPeriod = 10

-- COM and MON period
procPeriod :: Int
procPeriod = 2


-- Single Channel Wheel Brake Example ------------------------------------------

atom_wbs :: Atom ()
atom_wbs = atom "atom_wbs" $ do

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
  period buttonPeriod . atom "button" $ do
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

  let probeP nm e = probe (pName pp nm) e

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
  period procPeriod . atom "command" $ do
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
  period procPeriod . atom "monitor" $ do
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


-- | Invoke the atom compiler
compileToC :: IO ()
compileToC = do
    _ <- A.compile "atom_wbs" cfg atom_wbs
    return ()
  where
    cfg = defaults { cCode = prePostCode }
    prePostCode _ _ _ =
      ( unlines [ "#include <stdio.h>"
                , "#include <unistd.h>"
                , ""
                , "// ---- BEGIN of source automatically generated by Atom ----"
                ]
      , unlines [ "// ---- END of source automatically generated by Atom ----"
                , ""
                , "int main(int argc, char **argv) {"
                , "  // call guards() once per second"
                , "  while(1) { atom_wbs(); usleep(1000); }"
                , "}"
                ]
      )


-- Main -----------------------------------------------------------------

putHeader :: IO ()
putHeader = putStrLn (replicate 72 '-')

translateToSally :: (String, Atom (), TrConfig, String) -> IO ()
translateToSally (nm, spec, cfg, q) = do
  let fname = testDir </> nm ++ ".mcmt"
  compileToSally nm cfg fname spec (Just q)
  putStrLn ("compiled " ++ fname)


main :: IO ()
main = do

  putStrLn "Compiling atom_wbs to C... (atom_wbs.{c,h})"
  compileToC

  putStrLn "Compiling atom_wbs to Sally... (atom_wbs.mcmt)"
  translateToSally
    ("WBS", atom_wbs, defSpecCfg, "(query WBS_transition_system true)")
