-- |
-- Module: Language.LIMA.C.Compile
-- Description: Compilation functions
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
-- Copyright: (c) 2017 Benjamin Jones
--
-- LIMA compilation functions

module Language.LIMA.C.Compile
  ( compile
  , CompileResult(..)
  , reportSchedule
  , Schedule
  ) where

import System.Exit
import Control.Monad (when)
import Data.Maybe (isJust)

import Language.LIMA.Elaboration
import Language.LIMA.UeMap (emptyMap)
import Language.LIMA.Language hiding (Atom)

import Language.LIMA.C.Code
import Language.LIMA.C.Scheduling


-- | Package of all the compilation results
data CompileResult = CompileResult
  { compSchedule    :: Schedule        -- ^ schedule computed by the compiler
  , compCoverage    :: RuleCoverage    -- ^ rule coverage
  , compChans       :: [ChanInfo]      -- ^ channels used in the system
  , compAssertNames :: [Name]          -- ^ assertion statement names
  , compCoverNames  :: [Name]          -- ^ coverage statement names
  , compProbes      :: [(Name, Type)]  -- ^ declared probe names and types
  }

-- | Compiles an atom description to C.
compile :: Name
        -> Config
        -> Atom ()
        -> IO CompileResult
compile name config atom' = do
  -- TODO an Atom () -> Atom () rewriting step could be inserted here before
  -- elaboration
  res <- elaborate defCCtx emptyMap name atom'
  case res of
   Nothing -> putStrLn "ERROR: Design rule checks failed." >>
              exitWith (ExitFailure 1)
   Just (umap, (state, rules, chanIns, assertionNames, coverageNames, probeNames)) -> do
     -- main code generation step
     let sch = schedule rules umap
     ruleCoverage <- writeC name config state rules sch assertionNames
                     coverageNames probeNames

     when (isJust $ hardwareClock config) (putStrLn hwClockWarning)
     return $ CompileResult sch ruleCoverage chanIns assertionNames
                            coverageNames probeNames

hwClockWarning :: String
hwClockWarning = unlines
 [ ""
 , "*** LIMA WARNING: you are configuring to use a hardware clock.  Please remember"
 , "    to set the \"__phase_start_time\" variable to the time at which the first"
 , "    phase should be run before you enter the main LIMA-generated function the"
 , "    first time."
 ]
