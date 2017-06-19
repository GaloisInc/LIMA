-- |
-- Module      :  Language.LIMA.C
-- Copyright   :  Tom Hawkins & Lee Pike 2013 and Benjamin Jones 2017
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Re-export definitions from Language.LIMA.C.*
--

module Language.LIMA.C
  ( -- * Code
    -- | Module: "Language.LIMA.C.Code"
    Config (..), defaults, Clock (..), defaultClock, writeC, cType, RuleCoverage,
    -- * Compilation
    -- | Module: "Language.LIMA.C.Compile"
    compile, CompileResult(..), reportSchedule, Schedule,
    -- * Utilities
    -- | Module: "Language.LIMA.C.Util"
    printString, printIntegralE, printFloatingE, printProbe
  ) where

import Language.LIMA.C.Code
import Language.LIMA.C.Compile
import Language.LIMA.C.Util
