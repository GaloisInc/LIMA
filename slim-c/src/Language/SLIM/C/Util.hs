-- |
-- Module: Util
-- Description: reporting & debugging
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
-- Copyright: (c) 2017 Benjamin Jones
--
-- reporting & debugging for SLIM

module Language.SLIM.C.Util
  ( -- * Printing Utilities
    printString
  , printIntegralE
  , printFloatingE
  , printProbe
  ) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Text.Printf

import Language.SLIM


-- | Print a string in C using @printf@, appending a newline.
printString :: String -> Atom ()
printString s = action (\ _ -> "printf(\"" ++ s ++ "\\n\")") []

-- | Print an integral value in C using @printf@.
printIntegralE :: IntegralE a =>
                  String -- ^ Prefix for printed value
                  -> E a -- ^ Integral value to print
                  -> Atom ()
printIntegralE name' value' =
  action (\ v' -> "printf(\"" ++ name' ++ ": %i\\n\", " ++ head v' ++ ")")
  [ue value']

-- | Print a floating point value in C using @printf@.
printFloatingE :: FloatingE a =>
                  String -- ^ Prefix for printed value
                  -> E a -- ^ Floating point value to print
                  -> Atom ()
printFloatingE name' value' =
  action (\ v' -> "printf(\"" ++ name' ++ ": %f\\n\", " ++ head v' ++ ")")
  [ue value']

-- TODO: Factor out common code in the above - which is all of it, except
-- for a single character (%i vs. %f)

-- | Print the value of a probe to the console (along with its name).
printProbe :: (String, UE) -> Atom ()
printProbe (str, ue_) = case typeOf ue_ of
  Bool   -> printIntegralE str (ruInt   :: E Int8)
  Int8   -> printIntegralE str (ruInt   :: E Int8)
  Int16  -> printIntegralE str (ruInt   :: E Int16)
  Int32  -> printIntegralE str (ruInt   :: E Int32)
  Int64  -> printIntegralE str (ruInt   :: E Int64)
  Word8  -> printIntegralE str (ruInt   :: E Word8)
  Word16 -> printIntegralE str (ruInt   :: E Word16)
  Word32 -> printIntegralE str (ruInt   :: E Word32)
  Word64 -> printIntegralE str (ruInt   :: E Word64)
  Double -> printFloatingE str (ruFloat :: E Double)
  Float  -> printFloatingE str (ruFloat :: E Float)
  where ruInt   :: E a
        ruInt   =  Retype ue_
        ruFloat :: E a
        ruFloat =  Retype ue_
