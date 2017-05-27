-- |
-- Module      :  src.Language.SLIM.Graph
-- Copyright   :  Benjamin Jones 2017
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Produce GraphViz graphs from Atoms
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.SLIM.Graph (
    graphAtom
  , ex
) where

import           Control.Monad (forM_)
import           Data.Int
import           Data.GraphViz hiding (DotGraph)
import           Data.GraphViz.Types.Monadic
import           Data.GraphViz.Types.Generalised
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)

import Language.SLIM
import qualified Language.SLIM.Elaboration as E
import Language.SLIM.UeMap (emptyMap)


-- Atrributes for nodes and edges ----------------------------------------------

chanAttrs :: Attributes
chanAttrs = [color Purple, style solid]

subAtomArrAttrs :: Attributes
subAtomArrAttrs = [color Black, style dotted]

atomAttrs :: Attributes
atomAttrs = [textLabel "\\N", color Black, shape DiamondShape]


-- Graph producing functions ---------------------------------------------------

graphAtom :: FilePath -> Atom () -> IO ()
graphAtom fp atm = do
  g <- mkDotGraph atm
  fp' <- Data.GraphViz.addExtension (runGraphvizCommand Dot g) Png fp
  putStrLn ("wrote filename: " ++ fp')


mkDotGraph :: Atom () -> IO (DotGraph Text)
mkDotGraph atm = do
    let ((_a, _nts), (_u, (_g, adb))) = E.buildAtom E.defCCtx emptyMap
                                                    E.initialGlobal "top" atm
        tname = T.pack (E.atomName adb)
    res <- E.elaborate E.defCCtx emptyMap "top" atm
    let rules = case res of
          Nothing -> error "ERROR: Atom failed to compile."
          Just (_, (_, r, _, _, _, _)) -> r
    let ruleMap = Map.fromList [(E.ruleId r, r) | r@E.Rule{} <- rules]
    let cs = Map.toList $ E.getChannels rules

    return $
      digraph (Str tname) $ do
        flatten adb
        addChannels cs ruleMap

  where
    flatten adb = do
      let subs = E.atomSubs adb
      let n = T.pack (E.atomName adb)
      if null subs
         then
           node n atomAttrs
         else
           cluster (Str n) $ do
             node n atomAttrs
             let gps = map flatten subs
             sequence_ gps
             forM_ subs $ \s ->
               edge n (T.pack (E.atomName s)) subAtomArrAttrs

    addChannels cs rm = forM_ cs $ \(_cid, cinf) -> do
      let s0 = E.cinfoSrc cinf
      let r0 = E.cinfoRecv cinf
      case (s0, r0) of
        (Just s, Just r)   -> edge (lk rm s) (lk rm r) chanAttrs
        _                  -> return ()

    lk rm i = case Map.lookup i rm of
             Nothing -> error "ERROR: internal rule Id error"
             Just r -> T.pack (E.ruleName r)


-- | Example atom for testing purposes
ex :: Atom ()
ex = atom "ex" $ do
  (cin, cout) <- channel "phone" Bool

  atom "a1" $ do
    x <- bool "x" False
    x <== true
    writeChannel cin true

  atom "a2" $ do
    a <- int64 "a" (0 :: Int64)
    cond $ fullChannel cout
    _m <- readChannel cout
    incr a
