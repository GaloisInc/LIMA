-- |
-- Module: Util
-- Description: reporting & debugging
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
-- Copyright: (c) 2017 Benjamin Jones
--
-- reporting & debugging for LIMA

module Language.LIMA.C.Util
  ( -- * Printing Utilities
    printString
  , printE
  , printProbe
  ) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Text.Printf

import Language.LIMA


-- | Print a string in C using @printf@, appending a newline.
printString :: String -> Atom ()
printString s = action (\ _ -> "printf(\"" ++ s ++ "\\n\")") []

-- | Print an integral value in C using @printf@.
printE :: Expr a
       => String  -- ^ printf format string to use
       -> String  -- ^ Prefix for printed value
       -> E a     -- ^ Integral value to print
       -> Atom ()
printE fmt name' value' =
  action (\ v' -> concat ["printf(\"", name', ": %\" ", fmt, " \"\\n\", "
                         , head v', ")"
                         ])
  [ue value']

-- | Print the value of a probe to the console (along with its name).
printProbe :: (String, UE) -> Atom ()
printProbe (str, ue_) =
  let t = typeOf ue_
      fmt = printfFmt t
  in case t of
  Bool   -> printE fmt str (ruInt   :: E Int8)
  Int8   -> printE fmt str (ruInt   :: E Int8)
  Int16  -> printE fmt str (ruInt   :: E Int16)
  Int32  -> printE fmt str (ruInt   :: E Int32)
  Int64  -> printE fmt str (ruInt   :: E Int64)
  Word8  -> printE fmt str (ruInt   :: E Word8)
  Word16 -> printE fmt str (ruInt   :: E Word16)
  Word32 -> printE fmt str (ruInt   :: E Word32)
  Word64 -> printE fmt str (ruInt   :: E Word64)
  Double -> printE fmt str (ruFloat :: E Double)
  Float  -> printE fmt str (ruFloat :: E Float)
  where ruInt   :: E a
        ruInt   =  Retype ue_
        ruFloat :: E a
        ruFloat =  Retype ue_

printfFmt :: Type -> String
printfFmt Bool   = "\"d\""
printfFmt Int8   = "PRId8"  -- these macros require <inttypes.h>
printfFmt Int16  = "PRId16"
printfFmt Int32  = "PRId32"
printfFmt Int64  = "PRId64"
printfFmt Word8  = "PRIu8"
printfFmt Word16 = "PRIu16"
printfFmt Word32 = "PRIu32"
printfFmt Word64 = "PRIu64"
printfFmt Float  = "\"f\""
printfFmt Double = "\"f\""
