{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  Language.Sally.Expr
-- Copyright   :  Benjamin Jones <bjones@galois.com> 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Better constructors for Sally expresssions and predicates than the raw ones
-- defined in "Language.Sally.Types".
--
module Language.Sally.Expr (
    -- * better constructors
    boolExpr
  , boolPred
  , intExpr
  , zeroExpr
  , oneExpr
  , realExpr
  , addExpr
  , subExpr
  , multExpr
  , notExpr
  , eqExpr
  , neqExpr
  , ltExpr
  , leqExpr
  , gtExpr
  , geqExpr
  , muxExpr
  , andExprs
  , andPreds
  , orExprs
  , varExpr
  , varExpr'
  -- * complex expression builders
  , minExpr
  , countExpr
  -- * expression rewriting
  , constFold
  , simplifyAnds
  , simplifyOrs
  , flattenAnds
  , flattenOrs
) where

import Data.Sequence (Seq, (<|), (><), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Language.Sally.Types


-- Better Constructors ---------------------------------------------------------

boolExpr :: Bool -> SallyExpr
boolExpr = SELit . SConstBool

boolPred :: Bool -> SallyPred
boolPred = SPConst

intExpr :: Integral a => a -> SallyExpr
intExpr = SELit . SConstInt . fromIntegral

zeroExpr :: SallyExpr
zeroExpr = intExpr (0 :: Int)

oneExpr :: SallyExpr
oneExpr = intExpr (1 :: Int)

realExpr :: Real a => a -> SallyExpr
realExpr = SELit . SConstReal . toRational


-- | Better constructor for adding expressions
--   TODO maintain normal form
addExpr :: SallyExpr -> SallyExpr -> SallyExpr
addExpr x y = SEArith (SAAdd x y)

subExpr :: SallyExpr -> SallyExpr -> SallyExpr
subExpr x y = SEArith (SAAdd x ny)
  where ny = multExpr (SELit (SConstInt (-1))) y

-- | Better constructor for multiplying expressions; checks that one of the
-- operands is a constant.
multExpr :: SallyExpr -> SallyExpr -> SallyExpr
multExpr x y = if (isMultConst x || isMultConst y) then SEArith (SAMult x y)
               else error "multExpr: non-linear arithmetic is not supported"

-- | Note: this is an over approximation, e.g. (x + (-x))*y is a constant 0
-- times y, but will not pass this predicate.
isMultConst :: SallyExpr -> Bool
isMultConst (SELit _) = True
isMultConst (SEVar _) = False
isMultConst (SEPre _) = False
isMultConst (SEArith (SAAdd x y))  = isMultConst x && isMultConst y
isMultConst (SEArith (SAMult x y)) = isMultConst x && isMultConst y
isMultConst (SEArith (SAExpr _)) = False
isMultConst (SEMux{}) = False

eqExpr :: SallyExpr -> SallyExpr -> SallyExpr
eqExpr x y = SEPre (SPEq x y)

ltExpr :: SallyExpr -> SallyExpr -> SallyExpr
ltExpr x y = SEPre (SPLt x y)

leqExpr :: SallyExpr -> SallyExpr -> SallyExpr
leqExpr x y = SEPre (SPLEq x y)

gtExpr :: SallyExpr -> SallyExpr -> SallyExpr
gtExpr x y = SEPre (SPGt x y)

geqExpr :: SallyExpr -> SallyExpr -> SallyExpr
geqExpr x y = SEPre (SPGEq x y)

notExpr :: SallyExpr -> SallyExpr
notExpr x = SEPre (SPNot (getPred x))

neqExpr :: SallyExpr -> SallyExpr -> SallyExpr
neqExpr x y = notExpr (eqExpr x y)

-- | Turn a SallyExpr into a SallyPred (if possible)
getPred :: SallyExpr -> SallyPred
getPred x = case x of
              SEPre w   -> w
              SELit{}   -> SPExpr x
              SEVar{}   -> SPExpr x
              SEMux{}   -> SPExpr x
              SEArith{} -> error ("notExpr: cannot turn expression into predicate: "
                                 ++ show x)

muxExpr :: SallyExpr -> SallyExpr -> SallyExpr -> SallyExpr
muxExpr = SEMux

andExprs :: [SallyExpr] -> SallyExpr
andExprs es = SEPre $ andPreds (fmap getPred es)

andPreds :: [SallyPred] -> SallyPred
andPreds = SPAnd . flattenAnds . Seq.fromList

orExprs :: [SallyExpr] -> SallyExpr
orExprs es = SEPre $ orPreds (fmap getPred es)

orPreds :: [SallyPred] -> SallyPred
orPreds = SPOr . flattenOrs . Seq.fromList

varExpr :: SallyVar -> SallyExpr
varExpr = SEVar

varExpr' :: Name -> SallyExpr
varExpr' = SEVar . varFromName


-- More Complicated expression builders ----------------------------------------

-- | Given a non-empty finite list of expressions, build an expression to
-- compute their minimum. The second argument is a special value which, if
-- present causes expressions in the list with this value to be ignored in the
-- calculation. If the input list contains only the special value, then the
-- special value itself is returned.
minExpr :: [SallyExpr] -> Maybe SallyExpr -> SallyExpr
minExpr [] _ = error "minExpr: cannot apply minExpr to empty list"
minExpr (x:rest) sp' = go sp' x rest
  where go _ m [] = m
        go Nothing m (y:more) = muxExpr (ltExpr m y)
                                        (go sp' m more)
                                        (go sp' y more)
        go (Just sp) m (y:more) = muxExpr (andExprs [ltExpr m y, neqExpr m sp])
                                          (go sp' m more)
                                          (go sp' y more)

-- | Build a Sally expression representing the number of times a particular
-- item appears in a list of expressions.
countExpr :: SallyExpr -> [SallyExpr] -> SallyExpr
countExpr _ [] = zeroExpr
countExpr x (y:rest) = muxExpr (eqExpr x y) (addExpr oneExpr (countExpr x rest))
                                            (countExpr x rest)


-- Expression Rewriting --------------------------------------------------------

-- | A basic top-down recursive constant folding function.
constFold :: SallyExpr -> SallyExpr
constFold = simplifyExpr . constFold'
  where
    constFold' e@(SELit _) = e
    constFold' e@(SEVar _) = e
    constFold' (SEPre p) = SEPre (constFoldP p)
    constFold' (SEArith a) = SEArith (constFoldA a)
    constFold' (SEMux i t e) = constFoldM i t e

constFoldP :: SallyPred -> SallyPred
constFoldP = simplifyOrs . simplifyAnds

constFoldA :: SallyArith -> SallyArith
-- additive folding
--   add zero
constFoldA (SAAdd (SELit (SConstInt 0)) e)  = SAExpr (constFold e)
constFoldA (SAAdd e (SELit (SConstInt 0)))  = SAExpr (constFold e)
constFoldA (SAAdd (SELit (SConstReal 0)) e) = SAExpr (constFold e)
constFoldA (SAAdd e (SELit (SConstReal 0))) = SAExpr (constFold e)
--  add two constant literals
constFoldA (SAAdd (SELit (SConstInt x)) (SELit (SConstInt y))) =
  SAExpr (SELit (SConstInt (x+y)))
constFoldA (SAAdd (SELit (SConstReal x)) (SELit (SConstReal y))) =
  SAExpr (SELit (SConstReal (x+y)))
-- additive fall through case
constFoldA a@(SAAdd _ _) = a
-- multiplicitive folding:
--   mult by 1
constFoldA (SAMult (SELit (SConstInt 1)) e)  = SAExpr (constFold e)
constFoldA (SAMult e (SELit (SConstInt 1)))  = SAExpr (constFold e)
constFoldA (SAMult (SELit (SConstReal 1)) e) = SAExpr (constFold e)
constFoldA (SAMult e (SELit (SConstReal 1))) = SAExpr (constFold e)
--   mult by 0
constFoldA (SAMult (SELit (SConstInt 0)) _)  = SAExpr zeroExpr
constFoldA (SAMult _ (SELit (SConstInt 0)))  = SAExpr zeroExpr
constFoldA (SAMult (SELit (SConstReal 0)) _) = SAExpr zeroExpr
constFoldA (SAMult _ (SELit (SConstReal 0))) = SAExpr zeroExpr
--  mult two constant literals
constFoldA (SAMult (SELit (SConstInt x)) (SELit (SConstInt y))) =
  SAExpr (SELit (SConstInt (x*y)))
constFoldA (SAMult (SELit (SConstReal x)) (SELit (SConstReal y))) =
  SAExpr (SELit (SConstReal (x*y)))
--  fall through general case
constFoldA a@(SAMult _ _) = a
constFoldA (SAExpr e) = SAExpr (constFold e)

constFoldM :: SallyExpr ->  SallyExpr -> SallyExpr -> SallyExpr
constFoldM (SELit (SConstBool True)) t _  = constFold t
constFoldM (SELit (SConstBool False)) _ f = constFold f
constFoldM i t e = SEMux i (constFold t) (constFold e)

flattenAnds :: Seq SallyPred -> Seq SallyPred
flattenAnds (viewl -> xs) =
  case xs of
    EmptyL -> Seq.empty
    a :< rest  ->
      case a of
        SPAnd ys -> flattenAnds ys >< flattenAnds rest
        -- TODO enable rewriting here?
        -- SPConst True  -> flattenAnds rest
        -- SPConst False -> a <| Seq.empty
        _ -> a <| flattenAnds rest

flattenOrs :: Seq SallyPred -> Seq SallyPred
flattenOrs (viewl -> EmptyL) = Seq.empty
flattenOrs (viewl -> a :< rest) =
  case a of
    SPOr ys -> flattenOrs ys >< flattenOrs rest
    _ -> a <| flattenOrs rest
flattenOrs _ = undefined  -- make compiler happy :)

-- | Top-down rewriting of 'and' terms including constant folding and
-- constructor reduction.
simplifyAnds :: SallyPred -> SallyPred
simplifyAnds p =
  case p of
    -- main case
    SPAnd xs ->
      let ys = flattenAnds (fmap simplifyAnds xs) :: Seq SallyPred
      in case viewl ys of
           EmptyL  -> SPConst True           -- empty 'and'
           z :< zs -> if Seq.null zs then z  -- single elt. 'and'
                      else SPAnd ys          -- multiple
    SPExpr (SEPre q) -> simplifyAnds q       -- strip off SPExpr . SEPre
    -- other cases
    SPConst _   -> p
    SPOr    xs  -> SPOr (fmap simplifyAnds xs)
    SPImpl  x y -> SPImpl (simplifyAnds x) (simplifyAnds y)
    SPNot   x   -> SPNot (simplifyAnds x)
    SPEq    x y -> SPEq (constFold x) (constFold y)
    SPLEq   x y -> SPLEq (constFold x) (constFold y)
    SPGEq   x y -> SPGEq (constFold x) (constFold y)
    SPLt    x y -> SPLt (constFold x) (constFold y)
    SPGt    x y -> SPGt (constFold x) (constFold y)
    SPExpr  e   -> SPExpr (constFold e)

-- | Top-down rewriting of 'or' terms including constant folding and
-- constructor reduction.
simplifyOrs :: SallyPred -> SallyPred
simplifyOrs p =
  case p of
    -- main case
    SPOr xs ->
      let ys = flattenOrs (fmap simplifyOrs xs)
      in case viewl ys of
           EmptyL  -> SPConst False          -- empty disjunction
           z :< zs -> if Seq.null zs then z  -- single term
                      else SPOr ys           -- multiple terms
    SPExpr (SEPre q) -> simplifyOrs q        -- strip off SPExpr . SEPre
    -- other cases
    SPConst _   -> p
    SPAnd   xs  -> SPAnd (fmap simplifyOrs xs)
    SPImpl  x y -> SPImpl (simplifyOrs x) (simplifyOrs y)
    SPNot   x   -> SPNot (simplifyOrs x)
    SPEq    x y -> SPEq (constFold x) (constFold y)
    SPLEq   x y -> SPLEq (constFold x) (constFold y)
    SPGEq   x y -> SPGEq (constFold x) (constFold y)
    SPLt    x y -> SPLt (constFold x) (constFold y)
    SPGt    x y -> SPGt (constFold x) (constFold y)
    SPExpr  e   -> SPExpr (constFold e)

-- | Reduce SallyExpr terms by removing redundant constructors.
simplifyExpr :: SallyExpr -> SallyExpr
simplifyExpr (SEArith (SAExpr e)) = simplifyExpr e
simplifyExpr (SEPre (SPExpr e)) = simplifyExpr e
simplifyExpr e = e
