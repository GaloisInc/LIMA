{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  SExpPP
-- Copyright   :  Benjamin F Jones 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This module gives a uniform way to pretty print s-expressions through a
-- typeclass 'ToSExp'.
--

module SExpPP (
  -- * S-expression pretty printing
    SExp(..)
  , ToSExp(..)
  , bareText
  -- * misc
  , sallyCom
) where

import Data.Text.Lazy (Text)
import Text.PrettyPrint.Leijen.Text


data SExp = SXBare Doc     -- ^ bare symbol or literal
          | SXList [SExp]  -- ^ (foo a b)

class ToSExp a where
  toSExp :: a -> SExp

  sxPretty :: a -> Doc
  sxPretty = sxPrettyDefault . toSExp

instance ToSExp SExp where
  toSExp = id

sxPrettyDefault :: SExp -> Doc
sxPrettyDefault (SXBare x) = x
sxPrettyDefault (SXList []) = lparen <> rparen
sxPrettyDefault (SXList xs) = parens . group . align . vsep . fmap sxPretty $ xs
-- sxPrettyDefault (SXList ll@(x:_)) = case x of
--   SXBare _ -> parens (hang' (fillSep (map sxPretty ll)))
--   SXList _ -> parens (fillSep (map sxPretty ll))

bareText :: Text -> SExp
bareText = SXBare . text


-- Misc Sally Specific Items ---------------------------------------------------

sallyCom :: Doc
sallyCom = text ";;"
