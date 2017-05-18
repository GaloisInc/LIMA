-- |
-- Module      :  Language.SLIM.C
-- Copyright   :  Tom Hawkins & Lee Pike 2013 and Benjamin Jones 2017
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Re-export definitions from Language.SLIM.C.*
--

module Language.SLIM.C
  ( -- * Code
    -- | Module: "Language.SLIM.C.Code"
    Config (..), defaults, Clock (..), defaultClock, writeC, cType, RuleCoverage,
    -- * Compilation
    -- | Module: "Language.SLIM.C.Compile"
    compile, CompileResult(..), reportSchedule, Schedule,
    -- * Utilities
    -- | Module: "Language.SLIM.C.Util"
    printString, printIntegralE, printFloatingE, printProbe
  ) where

import Language.SLIM.C.Code
import Language.SLIM.C.Compile
import Language.SLIM.C.Util
