module Language.LIMA.Inspect
  ( ppAtomDB
    -- * re-export from Text.PrettyPrint
  , render
  )
where

import Text.PrettyPrint

import Language.LIMA.Elaboration
import Language.LIMA.UeMap

ppAtomDB :: AtomDB -> Doc
ppAtomDB a =
    text "AtomDB" <+> text (atomName a) <+> parens (int (atomId a)) $$
      nest 2 (
        text "atom enable =" <+> int (atomEnable a)  $$
        text "period      =" <+> int (atomPeriod a)  $$
        text "phase       =" <+> ppPhase (atomPhase a)  $$
        text "assigns:"                                 $$
          nest 2 (vcat (map ppAssign (atomAssigns a)))  $$
        text "sub atoms:" <+> hcat (map (int . atomId) (atomSubs a)))

ppAssign :: (MUV, Hash) -> Doc
ppAssign (m,h) = text "assign" <> parens (ppMUV m <> comma <> int h)

ppPhase :: Phase -> Doc
ppPhase = text . show

ppMUV :: MUV -> Doc
ppMUV = text . show

