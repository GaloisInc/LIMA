-- |
-- Module      :  Language.Sally.Translation
-- Copyright   :  Galois, Inc. 2016
-- License     :  BSD3
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Translation from Atom's AST to Sally's AST.
--
{-# LANGUAGE OverloadedStrings #-}

module Language.Sally.Translation (
    translaborate
  , TrConfig(..)
) where

import           Control.Arrow (second, (***))
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Sequence ((><), (|>))
import qualified Data.Sequence as Seq
import           Data.List ((\\))
import qualified Data.Text.Lazy as T
import           System.Exit

import qualified Language.LIMA.Analysis as AAna
import qualified Language.LIMA.Channel.Types as ACTyp
import qualified Language.LIMA.Elaboration as AEla
import qualified Language.LIMA.Expressions as AExp
import qualified Language.LIMA.Types as ATyp
import qualified Language.LIMA.UeMap as AUe

import           Language.Sally.Config
import           Language.Sally.Expr
import           Language.Sally.FaultModel
import           Language.Sally.PPrint (spPrint)
import           Language.Sally.Types


-- Entry Point from Atom -------------------------------------------------------

-- | Elaborate and translate an atom description to Sally. The 'TrResult' can
-- then be printed or written to disk.
translaborate :: Name
              -> TrConfig
              -> AEla.Atom ()
              -> IO TrResult
translaborate name config atm = do
  let aname = T.unpack . textFromName $ name
  res <- AEla.elaborate AEla.defSCtx AUe.emptyMap aname atm
  case res of
   Nothing -> do
     putStrLn "ERROR: Design rule checks failed."
     exitWith (ExitFailure 1)
   Just (umap, (state, rules, chans, _ass, _cov, _prob)) ->
     return (translate config name state umap rules chans)


-- Main Translation Code -------------------------------------------------------

-- | Main translation function.
translate :: TrConfig
          -> Name
          -> AEla.StateHierarchy
          -> AUe.UeMap
          -> [AEla.Rule]
          -> [AEla.ChanInfo]
          -> TrResult
translate conf name hier umap rules chans =
  TrResult { tresConsts   = tresConsts'
           , tresState    = tresState'
           , tresFormulas = tresFormulas'
           , tresInit     = tresInit'
           , tresTrans    = tresTrans'
           , tresSystem   = tresSystem'
           }
  where
    tresConsts'   = []  -- TODO support defined constants
    tresState'    = trState conf name hier rules chans
    tresFormulas' = trFormulas conf name hier rules chans
    tresInit'     = trInit conf name hier rules
    tresTrans'    = trRules conf name tresState' umap chans rules
    tresSystem'   = trSystem conf name

-- | Translate types from Atom to Sally. Currently the unsigned int /
-- bitvector types are not supported.
trType :: AExp.Type -> SallyBaseType
trType t = case t of
  AExp.Bool   -> SBool
  AExp.Int8   -> SInt
  AExp.Int16  -> SInt
  AExp.Int32  -> SInt
  AExp.Int64  -> SInt
  AExp.Float  -> SReal
  AExp.Double -> SReal
  AExp.Word8  -> SInt
  AExp.Word16 -> SInt
  AExp.Word32 -> SInt
  AExp.Word64 -> SInt

trTypeConst :: AExp.Const -> SallyBaseType
trTypeConst = trType . AExp.typeOf

trConst :: AExp.Const -> SallyConst
trConst (AExp.CBool   x) = SConstBool x
trConst (AExp.CInt8   x) = SConstInt  (fromIntegral x)
trConst (AExp.CInt16  x) = SConstInt  (fromIntegral x)
trConst (AExp.CInt32  x) = SConstInt  (fromIntegral x)
trConst (AExp.CInt64  x) = SConstInt  (fromIntegral x)
trConst (AExp.CWord8  x) = SConstInt  (fromIntegral x)
trConst (AExp.CWord16 x) = SConstInt  (fromIntegral x)
trConst (AExp.CWord32 x) = SConstInt  (fromIntegral x)
trConst (AExp.CWord64 x) = SConstInt  (fromIntegral x)
trConst (AExp.CFloat  x) = SConstReal (toRational x)
trConst (AExp.CDouble x) = SConstReal (toRational x)

trConstE :: AExp.Const -> SallyExpr
trConstE = SELit . trConst

-- | Define the default value to initialize variables of the given expression
-- type to.
trInitForType :: AExp.Type -> SallyExpr
trInitForType t = SELit $ case t of
  AExp.Bool   -> SConstBool False
  AExp.Int8   -> SConstInt 0
  AExp.Int16  -> SConstInt 0
  AExp.Int32  -> SConstInt 0
  AExp.Int64  -> SConstInt 0
  AExp.Word8  -> SConstInt 0
  AExp.Word16 -> SConstInt 0
  AExp.Word32 -> SConstInt 0
  AExp.Word64 -> SConstInt 0
  AExp.Float  -> SConstReal 0
  AExp.Double -> SConstReal 0

trName :: ATyp.Name -> Name
trName = nameFromS

-- | Produce a state type declaration from the 'StateHierarchy' in Atom.
-- Several 'input' variables are also synthesized here for the fault model.
trState :: TrConfig
        -> Name
        -> AEla.StateHierarchy
        -> [AEla.Rule]
        -> [AEla.ChanInfo]
        -> SallyState
trState conf name sh rules chans = SallyState (mkStateTypeName name) vars invars
  where
    invars = synthInvars  -- TODO expose input variables to DSL
    vars = (if AEla.isHierarchyEmpty sh then []
                                        else go Nothing sh)
           ++ faultStatusVars
           ++ clockVars
           ++ debugVars

    -- Recursive helper function to traverse the state hierarchy
    -- TODO (Maybe Name) for prefix is a little awkward here
    go :: Maybe Name -> AEla.StateHierarchy -> [(Name, SallyBaseType)]
    go prefix (AEla.StateHierarchy nm items) =
      concatMap (go (Just $ prefix `bangPrefix` uglyHack nm)) items
    go prefix (AEla.StateVariable nm c) =
      [(prefix `bangPrefix` uglyHack nm, trTypeConst c)]
    go prefix (AEla.StateChannel nm t) =
      let (chanVar, chanTime) = mkChanStateNames (prefix `bangPrefix` uglyHack nm)
      in [(chanVar, trType t), (chanTime, SReal)]
    go _prefix (AEla.StateArray _ _) = error "atom-sally does not yet support arrays"

    -- Declare input variables:
    --
    -- a) one (channel typed) input variable per CHANNEL, used to
    --    provide non-deterministic values on faulty channels.
    -- b) two input variables, one real, one integral, used to set message
    --    receipt times at the correct preiod and phase for each rule
    --    (transition). These are constrained by global assumptions.
    synthInvars =
      let cnm = AEla.cinfoName
          ctp = trType . AEla.cinfoType in
      concatMap (\c -> [ (mkFaultChanValueName (uglyHack (cnm c)), ctp c)
                       ]
                ) chans

    -- Declare one boolean state variable per NODE, these are partially
    -- constrained in the init block so that they remain at a constant value
    -- between 'faultTypeMin' and 'faultTypeMax' throughout a trace.
    faultStatusVars = [ (mkFaultNodeName name (AEla.ruleId r), SInt)
                      | r@AEla.Rule{} <- rules ]

    clockVars = [(mkClockTimeName name, SReal)]

    -- debug output consists of one variable: __last_transition  which
    -- indicates the transition # (aka rule Id) to be taken last
    debugVars = [ (mkLastTransName name, SInt) | cfgDebug conf ]

bangPrefix :: Maybe Name -> Name -> Name
bangPrefix mn n = maybe n (`bangNames` n) mn

-- | Produce a predicate describing the initial state of the system.
trInit :: TrConfig -> Name -> AEla.StateHierarchy -> [AEla.Rule] -> SallyStateFormula
trInit conf name sh rules = SallyStateFormula (mkInitStateName name)
                                               (mkStateTypeName name)
                                               spred
  where
    spred  = simplifyAnds $ SPAnd (Seq.fromList [ nodeInit
                                                , clockInit
                                                , assumptionsInit
                                                , debugInit
                                                , faultFlagConstraints
                                                ])

    nodeInit = if AEla.isHierarchyEmpty sh then SPConst True
                                           else go Nothing sh
    go :: Maybe Name -> AEla.StateHierarchy -> SallyPred
    go prefix (AEla.StateHierarchy nm items) =
      SPAnd (Seq.fromList $ map (go (Just $ prefix `bangPrefix` uglyHack nm)) items)
    go prefix (AEla.StateVariable nm c) =
      SPEq (varExpr' (prefix `bangPrefix` uglyHack nm)) (trConstE c)
    go prefix (AEla.StateChannel nm t) =
      let (chanVar, chanTime) = mkChanStateNames (prefix `bangPrefix` uglyHack nm)
      in SPAnd (  Seq.empty
               |> SPEq (varExpr' chanVar) (trInitForType t)
               |> SPEq (varExpr' chanTime) invalidTime)
    go _prefix (AEla.StateArray _ _) = error "atom-sally does not yet support arrays"

    clockInit = SPEq (varExpr' (mkClockTimeName name)) initialTime
    debugInit = if cfgDebug conf
                   then SPEq (varExpr' (mkLastTransName name)) initialLastTrans
                   else SPConst True
    assumptionsInit = SPExpr (varExpr' (mkAssumptionsFormulaName name))

    -- constrain node-fault type to values defined in "FaultModel"
    -- In the generated Sally model, 0 = NonFaulty, 1 = ManifestFaulty, etc..
    faultFlagConstraints =
      SPAnd (   Seq.fromList [SPLEq faultTypeMin' f | f <- faultExprs]
             >< Seq.fromList [SPLEq f faultTypeMax' | f <- faultExprs])
    faultExprs = [ varExpr' . mkFaultNodeName name . AEla.ruleId $ r
                 | r@AEla.Rule{} <- rules ]
    faultTypeMin' = intExpr faultTypeMin
    faultTypeMax' = intExpr faultTypeMax


-- | Collect the state type name, initial states name, and master transition
-- name into a 'SallySystem' record.
trSystem :: TrConfig -> Name -> SallySystem
trSystem _conf name = SallySystem (mkTSystemName name)
                                  (mkStateTypeName name)
                                  (mkInitStateName name)
                                  (mkMasterTransName name)

-- | Produce various state formulas which are used in transition relations and
-- queries.
trFormulas :: TrConfig
        -> Name
        -> AEla.StateHierarchy
        -> [AEla.Rule]
        -> [AEla.ChanInfo]
        -> [SallyStateFormula]
trFormulas conf name _sh rules _chans = masterList ++ [masterFormula]
  where                                -- ^ order matters here
    -- Conjunction of all the global assumptions
    masterList = [mfaFormula, lemmasFormula]
    masterPred = SPAnd (Seq.fromList fList)
      where fList = map (SPExpr . varExpr' . sfName) masterList
    masterFormula = SallyStateFormula (mkAssumptionsFormulaName name)
                                      (mkStateTypeName name)
                                      masterPred

    -- Formulas which constraint the fault model
    --
    mfaFormula = SallyStateFormula (mkMFAFormulaName name)
                                   (mkStateTypeName name)
                                   mfaPred
    -- node-fault predicate depends on which fault model we want
    mfaPred = case cfgMFA conf of
      NoFaults           -> fixedFaultsPred Map.empty
      HybridFaults ws wc -> weightedFaultsPred ws wc
      FixedFaults m      -> fixedFaultsPred m
    -- Hybrid fault model case
    weightedFaultsPred ws wc = SPExpr $ leqExpr (sumExpr ws wc) (intExpr numNodes)
    fts = [minBound..maxBound] :: [FaultType]  -- list of possible fault types
    nodeFaultE r@AEla.Rule{} = varExpr' (mkFaultNodeName name (AEla.ruleId r))
    nodeFaultE _ = error "trFormulas: nodeFault: no ruleId for non-Rule"
    faultStatusVars = map nodeFaultE realRules -- fault status variable names
    numNodes = length faultStatusVars
    -- given the weights, add an expr to the weighted fault count
    fn ws e f = if wgt == 0 then addExpr e (multExpr (intExpr wgt) cnt)
                            else e
      where wgt = fromMaybe 0 $ Map.lookup f ws
            cnt = countExpr (intExpr (fromEnum f)) faultStatusVars
    sumExpr ws wc = constFold $ addExpr (intExpr wc) (foldl' (fn ws) zeroExpr fts)
    -- Fixed fault mapping case
    fixedFaultsPred mp =
      case checkNodeNames mp of
        Nothing -> let ffConstraints = map (uncurry SPEq) (nodeFs mp)
                   in SPAnd (Seq.fromList ffConstraints)
        Just badNames ->
          error . unwords $ [ "trFormulas:\n  Names given in the FIXED FAULT"
                            , "MAPPING do not correspond to\n  system"
                            , "nodes: " ] ++ map spPrint badNames
    checkNodeNames mp =
      let nms = map nodeNm realRules
          badNames = filter (\k -> not (k `elem` nms)) (Map.keys mp)
      in if null badNames then Nothing
                          else Just badNames
    nodeNm r@AEla.Rule{} = mkNodeName name (uglyHack (AEla.ruleName r))
    nodeNm _        = error "trFormulas: nodeName: no name for non-Rule"
    lkFault r mp = intExpr
                 . fromEnum
                 . fromMaybe NonFaulty
                 $ Map.lookup (nodeNm r) mp
    nodeFs mp = [(nodeFaultE r, lkFault r mp) | r <- realRules]
    -- TODO emit an error if 'mp' contains names that are not node names
    realRules = [r | r@AEla.Rule{} <- rules]

    -- Lemmas that are automatically generated
    --
    lemmasFormula = SallyStateFormula (mkLemmasFormulaName name)
                                      (mkStateTypeName name)
                                      lemmasPred

    lemmasPred =
      let clkE = varExpr' (mkClockTimeName name)
          nonNegTime = SPLEq zeroExpr clkE  -- 0 <= global time
      in simplifyAnds . SPAnd . Seq.fromList $
           [ nonNegTime
             -- TODO: add more lemmas
           ]


-- | Translate Atom 'Rule's into 'SallyTransition's. One transition is
-- produced per rule, plus one clock transition, plus one master transition
-- for use in defining the transition system as a whole.
--
-- Note: Assertion and Coverage rules are ignored.
trRules :: TrConfig
        -> Name
        -> SallyState
        -> AUe.UeMap
        -> [AEla.ChanInfo]
        -> [AEla.Rule]
        -> [SallyTransition]
trRules conf name st umap chans rules = mapMaybe trRule rules
                                      ++ [clock, master]
  where trRule :: AEla.Rule -> Maybe SallyTransition
        trRule r@AEla.Rule{} = Just $ SallyTransition (mkTName r)
                                                      (mkStateTypeName name)
                                                      (mkLetBinds r)
                                                      (mkPred r)
        trRule _ = Nothing  -- skip assertions and coverage

        -- master transition is (for now) the disjunction of all minor
        -- transitions
        -- TODO add non-deterministic single-node transitions (Update: not clear if
        -- we really require single-node transitions. Allowing simultaneous-node
        -- transitions, as well as single-node is more general and simpler to
        -- encode..)
        master = SallyTransition (mkMasterTransName name)
                                 (mkStateTypeName name)
                                 []
                                 masterPred
        minorTrans = map (SPExpr . SEVar . varFromName . mkTName) rules
        nodeTrans = simplifyOrs $ SPOr (   Seq.fromList minorTrans
                                        |> SPExpr (varExpr' (mkClockTransName name)))
        invariant = SPExpr . varExpr' . nextName . mkAssumptionsFormulaName $ name
        masterPred = SPAnd . Seq.fromList $ [invariant, nodeTrans]

        clock = SallyTransition (mkClockTransName name)
                                (mkStateTypeName name)
                                []
                                clockPred
        -- to construct the clock predicate we call 'minExpr' which builds an
        -- expression representing the minimum time on the calendar
        -- (ignoring invalid times)
        clockPred =
          if not (null chans)
             then let m = minExpr calTimes (Just invalidTime)
                  in SPAnd $ (Seq.empty
                               |> SPLt (mkClockStateExpr  name) m
                               |> SPEq (mkClockStateExpr' name) m
                               |> (if cfgDebug conf  -- mark a clock transition
                                     then SPEq lastTransE' clockLastTrans
                                     else SPConst True))
                             >< clkLeftovers
             else boolPred False  -- case of no channels
        lastTransE' = varExpr' . nextName . mkLastTransName $ name
        -- variables updated by clock
        -- TODO: this leftovers mechanism is really clumsy. Whenever a new
        -- state variable is added the leftovers in every transition have to
        -- be modified...
        clkUsed = [mkClockTimeName name]
               ++ [ mkLastTransName name | cfgDebug conf ]
        clkLeftovers = Seq.fromList $ map handleLeftovers (stVars \\ clkUsed)
        calTimes = map ( varExpr' . stateName . snd . mkChanStateNames
                       . uglyHack . AEla.cinfoName
                       ) chans

        -- predicate that makes a variable stutter
        handleLeftovers n = SPEq (varExpr' (nextName n))
                                 (varExpr' (stateName n))
        -- all state variables
        stVars = map fst (sVars st)

        mkTName :: AEla.Rule -> Name
        mkTName r@AEla.Rule{} = mkTransitionName (AEla.ruleId r) name
        mkTName _ = error "impossible! assert or coverage rule found in mkTName"

        getUEs :: AEla.Rule -> [(AUe.Hash, SallyVar)]
        getUEs r = map (second trExprRef) . AAna.topo umap $ AEla.allUEs r

        mkLetBinds :: AEla.Rule -> [SallyLet]
        mkLetBinds r@AEla.Rule{} =
          let ues = getUEs r
          in map (\(h, v) -> (v, trUExpr name umap chans ues h)) ues
        mkLetBinds _ = error "impossible! assert or coverage rule found in mkLetBinds"

        -- TODO Avoid the ugly name mangling hack here by having variables
        -- in Atom carry not a name, but a structured heirarchy of names that
        -- can be flattened differently depending on the compile target
        mkPred :: AEla.Rule -> SallyPred
        mkPred r@AEla.Rule{} =
          let ues = getUEs r
              lkErr h = "trRules: failed to lookup untyped expr " ++ show h
              lk h = fromMaybe (error $ lkErr h) $ lookup h ues

              -- extract variabe name
              vName muv = case muv of
                AUe.MUV _ n _    -> uglyHack n
                AUe.MUVArray{}   -> error "trRules: arrays are not supported"
                AUe.MUVExtern{}  -> error "trRules: external vars are not supported"
                AUe.MUVChannel{} -> error ("trRules: Chan can't appear in lhs "
                                         ++"of assign, use 'writeChannel' instead.")
                AUe.MUVChannelReady{} ->
                  error "trRules: Chan can't appear in lhs of assign"

              -- translate assignments into equality between state and next
              -- vars
              handleAssign (muv, h) = SPEq (varExpr' (nextName . vName $ muv))
                                           (SEVar (lk  h))
              -- translate channel writes into a pair of assignements, one to
              -- the calendar entry value and one to the calendar entry
              -- timeout
              chanNames :: ACTyp.HasChan a => a -> (Name, Name)
              chanNames = mkChanStateNames . uglyHack . ACTyp.chanName

              enableExpr = andExprs [ SEVar (lk (AEla.ruleEnable r))
                                    , SEVar (lk (AEla.ruleEnableNH r))]

              handleChanWrite (cin, h, d) =
                let mkE = varExpr' . stateName
                    mkE' = varExpr' . nextName
                    csnms = chanNames cin
                    -- current and next variables for calendar entries
                    (_       , calTimeE) = (mkE *** mkE) csnms
                    (calValE', calTimeE') = (mkE' *** mkE') csnms
                    globTimeExpr = mkClockStateExpr name
                    -- handle channel writes with explicit delay
                    messageDelay = realExpr $
                      case d of
                        ACTyp.DelayDefault -> cfgMessageDelay conf
                        ACTyp.DelayTicks t -> fromIntegral t
                    -- timeout is current global time + message delay
                    laterTime = addExpr globTimeExpr messageDelay
                    -- propagate the rule's enable condition to each channel
                    -- write
                    newTimeExpr = muxExpr enableExpr laterTime calTimeE
                in SPAnd $ Seq.empty
                             |> SPEq calValE' (SEVar (lk h))  -- set chan value
                             |> SPEq calTimeE' newTimeExpr    -- set chan time
              -- translate channel reads into consumes in a single assignement to the
              -- calendar entry time
              handleChanConsume cout =
                let calTimeE  = varExpr' . stateName . snd . chanNames $ cout
                    calTimeE' = varExpr' . nextName . snd . chanNames $ cout
                    newTime = muxExpr enableExpr invalidTime calTimeE
                in SPEq calTimeE' newTime
              -- state vars in this rule
              stVarsUsed = map (vName . fst) (AEla.ruleAssigns r)
                        ++ map (\(c,_,_) -> fst (chanNames c)) (AEla.ruleChanWrite r)
                        ++ map (\(c,_,_) -> snd (chanNames c)) (AEla.ruleChanWrite r)
                        ++ map (snd . chanNames) (AEla.ruleChanRead r)
                        ++ [ mkLastTransName name | cfgDebug conf ]

              -- leftovers are vars not explicitly mentioned in the atom body,
              -- we need to make sure they stutter using 'handleLeftovers'
              leftovers = stVars \\ stVarsUsed

              debugOps = [ SPEq lastTransE' (intExpr (AEla.ruleId r)) | cfgDebug conf ]

              ops = map handleAssign (AEla.ruleAssigns r)
                 ++ map handleChanWrite (AEla.ruleChanWrite r)
                 ++ map handleChanConsume (AEla.ruleChanRead r)
                 ++ map handleLeftovers leftovers
                 ++ debugOps

          in simplifyAnds $ SPAnd (Seq.fromList ops)
        mkPred _ = error "impossible! assert or coverage rule found in mkPred"


-- | Translate Expressions
trUExpr :: Name                    -- ^ Atom name
        -> AUe.UeMap               -- ^ untyped expression map
        -> [AEla.ChanInfo]         -- ^ channel meta-data for all channels in system
        -> [(AUe.Hash, SallyVar)]  -- ^ pre-translated arguments to the expression head
        -> AUe.Hash                -- ^ hash of expression head
        -> SallyExpr
trUExpr name umap chans ues h =
  case AUe.getUE h umap of
    AUe.MUVRef (AUe.MUV _ k _)     -> varExpr' . stateName . uglyHack $ k
    AUe.MUVRef (AUe.MUVArray _ _)  -> aLangErr "arrays"
    AUe.MUVRef (AUe.MUVExtern k _) -> varExpr' . stateName . uglyHack $ k
    AUe.MUVRef (AUe.MUVChannel _ k _)    -> mkFaultCheck name chans k
    AUe.MUVRef (AUe.MUVChannelReady _ k) -> mkTimeCheck name k
    AUe.MUCast _ _     -> aLangErr "casting"
    AUe.MUConst x      -> SELit (trConst x)
    AUe.MUAdd _ _      -> addExpr a b
    AUe.MUSub _ _      -> subExpr a b
    AUe.MUMul _ _      -> multExpr a b
    AUe.MUDiv _ _      -> aLangErr "division"
    AUe.MUMod _ _      -> aLangErr "modular arithmetic"
    AUe.MUNot _        -> notExpr a
    AUe.MUAnd _        -> andExprs ops
    AUe.MUBWNot _      -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWAnd  _ _   -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWOr   _ _   -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWXor  _ _   -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWShiftL _ _ -> aLangErr "bitwise operations & bitvectors"
    AUe.MUBWShiftR _ _ -> aLangErr "bitwise operations & bitvectors"
    AUe.MUEq  _ _      -> eqExpr a b
    AUe.MULt  _ _      -> ltExpr a b
    AUe.MUMux _ _ _    -> muxExpr a b c
    AUe.MUF2B _        -> aLangErr "cast to Word32"
    AUe.MUD2B _        -> aLangErr "cast to Word64"
    AUe.MUB2F _        -> aLangErr "cast to Float"
    AUe.MUB2D _        -> aLangErr "cast to Double"
    -- math.h functions are not supported
    AUe.MUPi           -> mathHErr "M_PI"
    AUe.MUExp   _      -> mathHErr "exp"
    AUe.MULog   _      -> mathHErr "log"
    AUe.MUSqrt  _      -> mathHErr "sqrt"
    AUe.MUPow   _ _    -> mathHErr "pow"
    AUe.MUSin   _      -> mathHErr "sin"
    AUe.MUAsin  _      -> mathHErr "asin"
    AUe.MUCos   _      -> mathHErr "cos"
    AUe.MUAcos  _      -> mathHErr "acos"
    AUe.MUSinh  _      -> mathHErr "sinh"
    AUe.MUCosh  _      -> mathHErr "cosh"
    AUe.MUAsinh _      -> mathHErr "asinh"
    AUe.MUAcosh _      -> mathHErr "acosh"
    AUe.MUAtan  _      -> mathHErr "atan"
    AUe.MUAtanh _      -> mathHErr "atanh"
  where lkErr k = "trExpr: failed to lookup untyped expr " ++ show k
        lk k = fromMaybe (error $ lkErr k) $ lookup k ues
        aLangErr s = error $ "trExpr: Atom language feature " ++ s ++ " is not supported"
        mathHErr s = error $ "trExpr: math.h function " ++ s ++ " is not supported"
        ops = map (SEVar . lk) $ AUe.ueUpstream h umap
        a  = head ops
        b  = ops !! 1
        c  = ops !! 2

-- | Construct a preducate that checks the fault status of a sending node and
-- returns either a faulty value (depending on the fault type) or the value
-- stored in the calendar entry.
mkFaultCheck :: Name             -- ^ Atom name
             -> [AEla.ChanInfo]  -- ^ list of all channel info
             -> ATyp.Name        -- ^ channel name
             -> SallyExpr
mkFaultCheck name chans nm = muxExpr checkFault calVal faultVal
  where checkFault = SEPre $ SPEq (varExpr' (stateName (mkFaultNodeName name srcId)))
                                  (toSallyExpr NonFaulty)
        -- TODO clean up all these variable name expression compositions
        faultVal = varExpr' . inputName . mkFaultChanValueName . uglyHack $ nm
        calVal   = varExpr' . stateName . fst . mkChanStateNames . uglyHack $ nm
        srcId    = case filter (\c -> AEla.cinfoName c == nm) chans of
                     [c] -> fromMaybe (error "mkFaultCheck: channel has no receiver")
                                      (AEla.cinfoSrc c)
                     []  -> error ( "mkFaultCheck: found chan ref with no ChanInfo:\n"
                                 ++ "  chans: " ++ show chans ++ "\n"
                                 ++ "  name: " ++ show nm ++ "\n")
                     _   -> error "mkFaultCheck: found chan ref >= 1 ChanInfo"


-- | Construct a predicate that checks the given time for equality with the
-- global time.
mkTimeCheck :: Name       -- ^ Atom name
            -> ATyp.Name  -- ^ Channel name
            -> SallyExpr
mkTimeCheck anm cnm = SEPre $ SPEq chanTime (mkClockStateExpr anm)
  where chanTime   = varExpr' . stateName . snd . mkChanStateNames . uglyHack $ cnm

-- Calendar Automata Parameters ------------------------------------------------

-- | Initial value and place holder value for calendar time entries.
invalidTime :: SallyExpr
invalidTime = SELit $ SConstReal (-1)

-- | Initial value for 't' in the model.
initialTime :: SallyExpr
initialTime = SELit $ SConstReal 0

-- | Initial value for '__last_transition'.
initialLastTrans :: SallyExpr
initialLastTrans = SELit $ SConstInt (-2)

-- | Value for the clock transition in '__last_transition'.
clockLastTrans :: SallyExpr
clockLastTrans = SELit $ SConstInt (-1)


-- Name Generation Utilities ---------------------------------------------------

-- | name --> @name_state_type@
mkStateTypeName :: Name -> Name
mkStateTypeName = (`scoreNames` "state_type")

-- | name --> @name_initial_state@
mkInitStateName :: Name -> Name
mkInitStateName = (`scoreNames` "initial_state")

-- | name --> @name_transition@
mkMasterTransName :: Name -> Name
mkMasterTransName = (`scoreNames` "transition")

-- | name --> @name_clock_transition@
mkClockTransName :: Name -> Name
mkClockTransName = (`scoreNames` "clock_transition")

-- | name --> (@name!var@, @name!time@)
mkChanStateNames :: Name -> (Name, Name)
mkChanStateNames name = (chanVar, chanTime)
  where chanVar   = name `bangNames` "var"
        chanTime = name `bangNames` "time"

-- | i name --> @name_transition_i@
mkTransitionName :: Int -> Name -> Name
mkTransitionName i name = name `scoreNames` "transition" `scoreNames`
                          nameFromS (show i)

-- | cname --> @cname!fault@
mkFaultChanValueName :: Name -> Name
mkFaultChanValueName cnm =
  cnm `bangNames` "fault"

-- | name1 name2 --> @name1!name2@
mkNodeName :: Name -> Name -> Name
mkNodeName _nm = id

-- | name1 name2 --> @name!__last_transition@
mkLastTransName :: Name -> Name
mkLastTransName nm = nm `bangNames` "__last_transition"

-- | i name --> @name!fault_node!i@
mkFaultNodeName :: Name -> Int -> Name
mkFaultNodeName nm i =
  nm `bangNames` "__faulty_node" `bangNames` nameFromS (show i)

-- | Translate a shared expression reference (an Int) to a variable, e.g.
-- @temp!0@.
trExprRef :: Int -> SallyVar
trExprRef i = varFromName $ nameFromT "temp" `bangNames` nameFromS (show i)

-- | name --> name_transition_system
mkTSystemName :: Name -> Name
mkTSystemName = (`scoreNames` "transition_system")

mkAssumptionsFormulaName :: Name -> Name
mkAssumptionsFormulaName = (`scoreNames` "assumptions")

mkMFAFormulaName :: Name -> Name
mkMFAFormulaName = (`scoreNames` "mfa_formula")

mkLemmasFormulaName :: Name -> Name
mkLemmasFormulaName = (`scoreNames` "lemmas")

-- | Global clock value. Matches constant in "atom".
mkClockTimeName :: Name -> Name
mkClockTimeName nm = nm `bangNames` "__global_clock"

-- | clock state (with state namespace)
mkClockStateExpr :: Name -> SallyExpr
mkClockStateExpr = varExpr' . stateName . mkClockTimeName

-- | next clock state
mkClockStateExpr' :: Name -> SallyExpr
mkClockStateExpr' = varExpr' . nextName . mkClockTimeName

-- | s/\./!/g
uglyHack :: String -> Name
uglyHack = trName . map dotToBang
  where dotToBang '.' = '!'
        dotToBang c   = c
