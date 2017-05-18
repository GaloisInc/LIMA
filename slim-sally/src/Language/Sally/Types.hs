-- |
-- Module      :  Language.Sally.Types
-- Copyright   :  Benjamin Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Types reflecting the basic Sally input language sections and base types
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Sally.Types (
    -- * Name type
    Name
  , textFromName
  , nameFromT
  , nameFromS
  , catNamesWith
  , bangNames
  , scoreNames
  , nextName
  , stateName
  , inputName
  , varFromName
    -- * Base types
  , SallyBaseType(..)
  , SallyConst(..)
    -- * Types for defining transition systems
  , SallyState(..)
  , SallyPred(..)
  , SallyVar(..)
  , SallyArith(..)
  , SallyExpr(..)
  , ToSallyExpr(..)
  , SallyStateFormula(..)
  , SallyLet
  , SallyTransition(..)
  , SallySystem(..)
  , TrResult(..)
) where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Ratio (numerator, denominator)
import Data.Sequence (Seq)
import Data.String
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

import SExpPP


-- Name type for Sally namespaces and variables ------------------------------------

newtype Name = Name { textFromName :: Text }
  deriving (Show, Eq, Ord)

instance Pretty Name where
  pretty = text . textFromName

instance ToSExp Name where
  toSExp = SXBare . text . textFromName

nameFromS :: String -> Name
nameFromS = Name . T.pack

nameFromT :: Text -> Name
nameFromT = Name

instance IsString Name where
  fromString = nameFromS

-- | Concatenate names with the given seperating text in between
catNamesWith :: Text -> Name -> Name -> Name
catNamesWith sp a b = Name (textFromName a `T.append` sp `T.append` textFromName b)

-- | Concatenate names with a 'bang' separated in between
bangNames :: Name -> Name -> Name
bangNames = catNamesWith "!"

-- | Concatenate names with an 'underscore' separated in between
scoreNames :: Name -> Name -> Name
scoreNames = catNamesWith "_"

-- | Return the name of the given name in the "next" namespace
nextName :: Name -> Name
nextName = catNamesWith "" "next."

-- | Return the name of the given name in the "state" namespace
stateName :: Name -> Name
stateName = catNamesWith "" "state."

-- | Return the name of the given name in the "input" namespace
inputName :: Name -> Name
inputName = catNamesWith "" "input."


-- Constants and base types ----------------------------------------------------

-- | A defined constant. For our purposes, a real number is represented
-- (approximated) by an exact rational number.
data SallyConst = SConstBool Bool
                | SConstInt  Integer
                | SConstReal Rational
  deriving (Show, Eq)

instance ToSExp SallyConst where
  toSExp (SConstBool b) = SXBare $ if b then text "true" else text "false"
  toSExp (SConstInt  x) =
    let bare = SXBare $ integer x
    in if x >= 0 then bare
       else SXList [bare]  -- if x < 0, enclose in parens
  toSExp (SConstReal x) =
    let nx = numerator x
        dx = denominator x
    in if dx == 1 then toSExp (SConstInt nx)  -- special case integers
                  else SXList [ SXBare "/", toSExp (SConstInt nx)
                              , toSExp (SConstInt dx) ]

-- | Base data types in Sally: Booleans, (mathematical) Integers, and
-- (mathematical) Reals
data SallyBaseType = SBool
                   | SInt
                   | SReal
  deriving (Show, Eq)

instance ToSExp SallyBaseType where
  toSExp SBool = bareText "Bool"
  toSExp SInt  = bareText "Int"
  toSExp SReal = bareText "Real"


-- Untyped Expression AST for Sally --------------------------------------------

newtype SallyVar = SallyVar { textFromVar :: Text }
  deriving (Show, Eq)

instance ToSExp SallyVar where
  toSExp = SXBare . text . textFromVar

varFromName :: Name -> SallyVar
varFromName = SallyVar . textFromName

-- | Expressions
data SallyExpr = SELit   SallyConst              -- ^ constant literal
               | SEVar   SallyVar                -- ^ variable
               | SEPre   SallyPred               -- ^ boolean expression
               | SEArith SallyArith              -- ^ arithmetic expression
               | SEMux   SallyExpr SallyExpr SallyExpr  -- ^ if then else
  deriving (Show, Eq)

class ToSallyExpr a where
  toSallyExpr :: a -> SallyExpr

instance ToSExp SallyExpr where
  toSExp (SELit x)   = SXBare (sxPretty x)
  toSExp (SEVar x)   = SXBare (sxPretty x)
  toSExp (SEPre x)   = toSExp x
  toSExp (SEArith x) = toSExp x
  toSExp (SEMux x y z) = SXList [bareText "ite", toSExp x, toSExp y, toSExp z]

-- | Predicates
data SallyPred = SPConst Bool                    -- ^ boolean constant
               | SPExpr  SallyExpr               -- ^ a boolean valued expression
               | SPAnd   (Seq SallyPred)         -- ^ and
               | SPOr    (Seq SallyPred)         -- ^ or
               | SPImpl  SallyPred SallyPred     -- ^ implication
               | SPNot   SallyPred               -- ^ logical negation
               | SPEq    SallyExpr SallyExpr     -- ^ ==
               | SPLEq   SallyExpr SallyExpr     -- ^ <=
               | SPGEq   SallyExpr SallyExpr     -- ^ >=
               | SPLt    SallyExpr SallyExpr     -- ^ <
               | SPGt    SallyExpr SallyExpr     -- ^ >
  deriving (Show, Eq)

instance ToSExp SallyPred where
  toSExp (SPConst x)   = SXBare (text (if x then "true" else "false"))
  toSExp (SPExpr  x)   = SXBare (sxPretty x)
  toSExp (SPAnd   xs)  = SXList (bareText "and" : toList (fmap toSExp xs))
  toSExp (SPOr    xs)  = SXList (bareText "or"  : toList (fmap toSExp xs))
  toSExp (SPImpl  p q) = SXList [bareText "=>", toSExp p, toSExp q]
  toSExp (SPNot   p)   = SXList [bareText "not",  toSExp p]
  toSExp (SPEq    x y) = SXList [bareText "=",  toSExp x, toSExp y]
  toSExp (SPLEq   x y) = SXList [bareText "<=", toSExp x, toSExp y]
  toSExp (SPGEq   x y) = SXList [bareText ">=", toSExp x, toSExp y]
  toSExp (SPLt    x y) = SXList [bareText "<",  toSExp x, toSExp y]
  toSExp (SPGt    x y) = SXList [bareText "<",  toSExp x, toSExp y]

-- | Arithmetic terms
data SallyArith = SAAdd   SallyExpr SallyExpr  -- ^ addition
                | SAMult  SallyExpr SallyExpr  -- ^ constant mult
                | SAExpr  SallyExpr            -- ^ general expression
  deriving (Show, Eq)

instance ToSExp SallyArith where
  toSExp (SAAdd x y)  = SXList [bareText "+", toSExp x, toSExp y]
  toSExp (SAMult x y) = SXList [bareText "*", toSExp x, toSExp y]
  toSExp (SAExpr e) = toSExp e


-- Compound Sally Types --------------------------------------------------------

-- | The state type in Sally
--
-- This consists of 1) a name for the type, 2) a set of state variables (and
-- their associated base type) and, 3) (optionally) a set in input variabels
-- which are uninterpreted in the model; they can be thought of as varying
-- non-deterministically in any system trace.
data SallyState = SallyState
  { sName   :: Name                     -- ^ state type name
  , sVars   :: [(Name, SallyBaseType)]  -- ^ state variables
  , sInVars :: [(Name, SallyBaseType)]  -- ^ state input variables
  }
  deriving (Show, Eq)

instance ToSExp SallyState where
  toSExp (SallyState {sName=sn, sVars=sv, sInVars=siv}) =
    SXList $ [ bareText "define-state-type"
             , toSExp sn
             , SXList $ map (\(n,t) -> SXList [toSExp n, toSExp t]) sv
             ] ++
             (if null siv then []
              else [SXList $ map (\(n,t) -> SXList [toSExp n, toSExp t]) siv])

-- | A named formula over a state type
data SallyStateFormula = SallyStateFormula
  { sfName   :: Name        -- ^ state formula name
  , sfDomain :: Name        -- ^ state formula domain
  , sfPred   :: SallyPred   -- ^ state formula predicate
  }
  deriving (Show, Eq)

instance ToSExp SallyStateFormula where
  toSExp (SallyStateFormula {sfName=sn, sfDomain=sd, sfPred=sp}) =
    SXList [ bareText "define-states"
           , toSExp sn
           , toSExp sd
           , toSExp sp
           ]

-- | A "let" binding: each let binds a 'SallyVar' to a Sally expression,
-- which can be a constant literal, a predicate (boolean value), or an
-- arithmetic expression.
type SallyLet = (SallyVar, SallyExpr)

-- | A transition over a given state type
data SallyTransition = SallyTransition
  { traName :: Name        -- ^ transition name
  , traDom  :: Name        -- ^ transition domain
  , traLet  :: [SallyLet]  -- ^ bindings for the transition relation
  , traPred :: SallyPred   -- ^ transition relation
  }
  deriving (Show, Eq)

instance ToSExp SallyTransition where
  toSExp (SallyTransition {traName=tn, traDom=td, traLet=tl, traPred=tp}) =
      SXList $ [ bareText "define-transition"
               , toSExp tn
               , toSExp td
               ] ++
               (if null listOfBinds then [toSExp tp]
                else [SXList [bareText "let", SXList listOfBinds, toSExp tp]])
    where
      listOfBinds = map (\(v,e) -> SXList [toSExp v, toSExp e]) tl

-- | A transition system declaration
data SallySystem = SallySystem
  { sysNm  :: Name  -- ^ system name
  , sysSN  :: Name  -- ^ system state name
  , sysISN :: Name  -- ^ system init state name
  , sysTN  :: Name  -- ^ system transition name
  }
  deriving (Show, Eq)

-- | Pretty print a 'SallySystem'.
instance ToSExp SallySystem where
    toSExp ss = SXList [ bareText "define-transition-system"
                       , toSExp (sysNm ss)
                       , toSExp (sysSN ss)
                       , toSExp (sysISN ss)
                       , toSExp (sysTN ss)
                       ]


-- Translation Results ---------------------------------------------------------

-- | The result of translation, a specific form of the Sally AST.
data TrResult = TrResult
  { tresState    :: SallyState           -- ^ system state variables
  , tresFormulas :: [SallyStateFormula]  -- ^ state formulas used in transitions
                                         --   and queries
  , tresConsts   :: [SallyConst]         -- ^ declared constants
  , tresInit     :: SallyStateFormula    -- ^ initialization formula
  , tresTrans    :: [SallyTransition]    -- ^ system transitions
  , tresSystem   :: SallySystem          -- ^ system definition
  }
  deriving (Show, Eq)

-- | TrResult requires a special printer since it is not an s-expression. The
-- order of the 'vcat' items is important because Sally is sensitive to names
-- being declared before they are used in a model file.
instance Pretty TrResult where
  pretty tr = vcat [ consts_comment
                   , consts
                   , state_comment
                   , sxPretty (tresState tr)
                   ] <$$>
              vcat (formulas_comment : intersperse
                                         sallyCom
                                         (map sxPretty (tresFormulas tr))) <$$>
              -- needs to come after formulas
              vcat [ init_comment
                   , sxPretty (tresInit tr)
                   ] <$$>
              -- needs to come after state, init, and formulas
              vcat (trans_comment : intersperse
                                      sallyCom
                                      (map sxPretty (tresTrans tr))) <$$>
              -- needs to come last
              vcat (system_comment : [sxPretty (tresSystem tr)])
    where
      consts = if null (tresConsts tr) then text ";; NONE"
               else vcat (map sxPretty (tresConsts tr))
      consts_comment = sallyCom <+> text "Constants"
      state_comment  = linebreak <> sallyCom <+> text "State type"
      init_comment   = linebreak <> sallyCom <+> text "Initial State"
      formulas_comment = linebreak <> sallyCom <+> text "State Formulas"
      trans_comment  = linebreak <> sallyCom <+> text "Transitions"
      system_comment = linebreak <> sallyCom <+> text "System Definition"

