-- |
-- Module: Analysis
-- Description: -
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike

module Language.LIMA.Analysis
  ( topo
  , ruleComplexity
  ) where

import Language.LIMA.Elaboration
import Language.LIMA.UeMap

-- | Topologically sorts a list of expressions and subexpressions.
topo :: UeMap -> [Hash] -> [(Hash, Int)]
topo mp ues = reverse ues'
  where
  start = 0
  (_, ues') = foldl collect (start, []) ues
  collect :: (Int, [(Hash, Int)]) -> Hash -> (Int, [(Hash, Int)])
  collect (n, ues_) ue | any ((== ue) . fst) ues_ = (n, ues_)
  collect (n, ues_) ue = (n' + 1, (ue, n') : ues'')
    where (n', ues'') = foldl collect (n, ues_) $ ueUpstream ue mp

-- | Number of UE's computed in rule.
ruleComplexity :: UeMap -> Rule -> Int
ruleComplexity mp = length . (topo mp) . allUEs

