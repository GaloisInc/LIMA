-- |
-- Module      :  Language.Sally.PPrint
-- Copyright   :  Benjamin Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  benjaminfjones@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Exports a pretty printer for the result of translation.
--
module Language.Sally.PPrint (
  -- * pretty printing
    spPrint
  , pprintSystem
  , putSystem
  , putSystemLn
  , hPutSystem
) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as BS

import System.IO (Handle)
import Text.PrettyPrint.Leijen.Text


spPrint :: Pretty a => a -> String
spPrint = L.unpack . pprintSystem

pprintSystem :: Pretty a => a -> L.Text
pprintSystem = displayT . renderPretty ribbon wid . pretty
  where ribbon = 72 / 80 :: Float
        wid    = 80

putSystem :: Pretty a => a -> IO ()
putSystem = putDoc . pretty

putSystemLn :: Pretty a => a -> IO ()
putSystemLn tr = putSystem tr >> putStrLn ""

hPutSystem :: Pretty a => Handle -> a -> IO ()
hPutSystem h tr = BS.hPutStr h . E.encodeUtf8 . pprintSystem $ tr
