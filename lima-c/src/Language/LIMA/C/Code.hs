-- |
-- Module: Language.LIMA.C.Code
-- Description: C code configuration and generation
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
-- Copyright: (c) 2017 Benjamin Jones
--
-- LIMA C code configuration and generation

module Language.LIMA.C.Code
  ( Config (..)
  , Clock (..)
  , writeC
  , defaults
  , defaultClock
  , cType
  , RuleCoverage
  ) where

import Debug.Trace

import Control.Arrow (second)
import Data.List
import Data.Maybe
import Text.Printf
import Data.Word
import qualified Data.Bimap as M

import Language.LIMA.Analysis
import Language.LIMA.Channel.Types
import Language.LIMA.Elaboration
import Language.LIMA.Expressions hiding (typeOf)
import qualified Language.LIMA.Expressions as E
import Language.LIMA.UeMap
import Language.LIMA.Types

import Language.LIMA.C.Scheduling

-- | C code configuration parameters.
data Config = Config
  { -- | Alternative primary function name.  If this is empty, then it will
    -- default to the name passed to 'Language.LIMA.C.Compile.compile'.
    cFuncName     :: String
    -- | Name of state variable structure. Default: @state@
  , cStateName    :: String
    -- | Custom C code to insert above and below the functions, given
    -- assertion names, coverage names, and probe names and types.
  , cCode         :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
    -- | Custom C code to insert above and below the state definition in the
    -- header file, given assertion names, coverage names, and probe names and
    -- types.
  , hCode         :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
    -- | Enable rule coverage tracking.
  , cRuleCoverage :: Bool
    -- | Enable assertions and functional coverage.
  , cAssert       :: Bool
    -- | Name of assertion function. Prototype:
    -- @void assert(int, bool, uint64_t);@
  , cAssertName   :: String
    -- | Name of coverage function. Prototype:
    -- @void cover(int, bool, uint64_t);@
  , cCoverName    :: String
    -- | Hardware counter to schedule rules, or 'Nothing' (the default).
  , hardwareClock :: Maybe Clock
  }

-- | Data associated with sampling a hardware clock.  For the clock to work
-- correctly, you MUST assign @__global_clock@ the current time (according to
-- @clockName@) the first time you enter the main LIMA-generated function
-- calling your rules.
data Clock = Clock
  { -- | C function to sample the clock.  The function is assumed to have the
    -- prototype: @clockType clockName(void)@.
    clockName  :: String
    -- | Clock type.  Assumed to be one of 'Word8', 'Word16', 'Word32', or
    -- 'Word64'.  It is permissible for the clock to roll over.
  , clockType  :: Type
    -- | Number of ticks in a phase.  Must be greater than 0.
  , delta      :: Integer
    -- | C function to delay/sleep.  The function is assumed to have the
    -- prototype: @void delay(clockType i)@, where @i@ is the duration of
    -- delay/sleep.
  , delay      :: String
    -- | 'Nothing', or a user-defined error-reporting function if the period
    -- duration is violated, e.g., the execution time was greater than @delta@.
    -- Assumed to have prototype: @void err(void)@.
  , err        :: Maybe String
  }

-- | Default C code configuration parameters (default function name, no
-- pre/post code, ANSI C types).
defaults :: Config
defaults = Config
  { cFuncName     = ""
  , cStateName    = "state"
  , cCode         = \ _ _ _ -> ("", "")
  , hCode         = \ _ _ _ -> ("", "")
  , cRuleCoverage = True
  , cAssert       = True
  , cAssertName   = "assert"
  , cCoverName    = "cover"
  , hardwareClock = Nothing
  }

-- | Default hardware clock parameters (name "@clk@", Word64, delta 1, delay
-- function is "@delay@", no error function).
defaultClock :: Clock
defaultClock = Clock { clockName = "clk"
                     , clockType = Word64
                     , delta = 1
                     , delay = "delay"
                     , err = Nothing
                     }

-- | Render constant literals
showConst :: Const -> String
showConst c = case c of
  CBool True  -> "true"
  CBool False -> "false"
  CInt8   a   -> show a
  CInt16  a   -> show a
  CInt32  a   -> show a ++ "L"
  CInt64  a   -> show a ++ "LL"
  CWord8  a   -> show a
  CWord16 a   -> show a
  CWord32 a   -> show a ++ "UL"
  CWord64 a   -> show a ++ "ULL"
  CFloat  a   -> show a ++ "F"
  CDouble a   -> show a


-- | C99 type naming rules.
cType :: Type -> String
cType t = case t of
  Bool   -> "bool"
  Int8   -> "int8_t"
  Int16  -> "int16_t"
  Int32  -> "int32_t"
  Int64  -> "int64_t"
  Word8  -> "uint8_t"
  Word16 -> "uint16_t"
  Word32 -> "uint32_t"
  Word64 -> "uint64_t"
  Float  -> "float"
  Double -> "double"

-- | Generate C code for the declaration of a given named, untyped expression
codeUE :: UeMap             -- ^ untyped expression map
       -> Config            -- ^ C code generator configuration
       -> [(Hash, String)]  -- ^ hash, name pairs for the operands of the expr
       -> String            -- ^ declaration prefix (e.g. static)
       -> (Hash, String)    -- ^ hash, name of expression to render
       -> String
codeUE mp config ues d (ue', n) =
  d ++ cType (typeOf ue' mp) ++ " " ++ n ++ " = " ++ basic ++ ";\n"
  where
  operands = map (fromJust . flip lookup ues) $ ueUpstream ue' mp
  basic :: String
  basic = concat $ case getUE ue' mp of
    MUVRef (MUV _ k _)                 -> [cStateName config, ".", k]
    MUVRef (MUVArray (UA _ k _) _)     -> [cStateName config, ".", k, "[", a, "]"]
    MUVRef (MUVArray (UAExtern k _) _) -> [k, "[", a, "]"]
    MUVRef (MUVExtern k _)             -> [k]
    MUVRef (MUVChannel _ k _)          -> [cStateName config, ".", chanVarCName k]
    MUVRef (MUVChannelReady _ k)       -> [cStateName config, ".", chanReadyVarCName k]
    MUCast _ _     -> ["(", cType (typeOf ue' mp), ") ", a]
    MUConst c_     -> [showConst c_]
    MUAdd _ _      -> [a, " + ", b]
    MUSub _ _      -> [a, " - ", b]
    MUMul _ _      -> [a, " * ", b]
    MUDiv _ _      -> [a, " / ", b]
    MUMod _ _      -> [a, " % ", b]
    MUNot _        -> ["! ", a]
    MUAnd _        -> intersperse " && " operands
    MUBWNot _      -> ["~ ", a]
    MUBWAnd  _ _   -> [a, " & ", b]
    MUBWOr   _ _   -> [a, " | ", b]
    MUBWXor  _ _   -> [a, " ^ ", b]
    MUBWShiftL _ _ -> [a, " << ", b]
    MUBWShiftR _ _ -> [a, " >> ", b]
    MUEq  _ _      -> [a, " == ", b]
    MULt  _ _      -> [a, " < " , b]
    MUMux _ _ _    -> [a, " ? " , b, " : ", c]
    MUF2B _        -> ["*((", ct Word32, " *) &(", a, "))"]
    MUD2B _        -> ["*((", ct Word64, " *) &(", a, "))"]
    MUB2F _        -> ["*((", ct Float , " *) &(", a, "))"]
    MUB2D _        -> ["*((", ct Double, " *) &(", a, "))"]
    -- math.h:
    MUPi           -> [ "M_PI" ]
    MUExp   _      -> [ "exp",   f, " ( ", a, " )"]
    MULog   _      -> [ "log",   f, " ( ", a, " )"]
    MUSqrt  _      -> [ "sqrt",  f, " ( ", a, " )"]
    MUPow   _ _    -> [ "pow",   f, " ( ", a, ", ", b, " )"]
    MUSin   _      -> [ "sin",   f, " ( ", a, " )"]
    MUAsin  _      -> [ "asin",  f, " ( ", a, " )"]
    MUCos   _      -> [ "cos",   f, " ( ", a, " )"]
    MUAcos  _      -> [ "acos",  f, " ( ", a, " )"]
    MUSinh  _      -> [ "sinh",  f, " ( ", a, " )"]
    MUCosh  _      -> [ "cosh",  f, " ( ", a, " )"]
    MUAsinh _      -> [ "asinh", f, " ( ", a, " )"]
    MUAcosh _      -> [ "acosh", f, " ( ", a, " )"]
    MUAtan  _      -> [ "atan",  f, " ( ", a, " )"]
    MUAtanh _      -> [ "atanh", f, " ( ", a, " )"]
    where
      ct = cType
      a = head operands
      b = operands !! 1
      c = operands !! 2
      f = case typeOf ue' mp of
            Float     -> "f"
            Double    -> ""
            _         -> error "unhandled float type"

type RuleCoverage = [(Name, Int, Int)]

-- | The top-level C code generator
writeC
  :: Name            -- ^ module name
  -> Config          -- ^ code gen configuration parameters
  -> StateHierarchy  -- ^ representation of the global state struct
  -> [Rule]          -- ^ list of atom rules to generate code for
  -> Schedule        -- ^ pre-computed execution schedule; includes UeMap
  -> [Name]          -- ^ assertion names
  -> [Name]          -- ^ coverage names
  -> [(Name, Type)]  -- ^ probe name, type pairs
  -> IO RuleCoverage
writeC name config state rules (mp, schedule') assertNames coverNames probeNames = do
    writeFile (name ++ ".c") c
    writeFile (name ++ ".h") h
    return [ (ruleName r, div (ruleId r) 32, mod (ruleId r) 32) | r <- rules' ]
  where
    (preCode,  postCode)  = cCode config assertNames coverNames probeNames
    (preHCode, postHCode) = hCode config assertNames coverNames probeNames

    -- lines of C code to write to file
    c = unlines
      [ -- includes
        "#include <stdbool.h>"
      , "#include <stdint.h>"
      , "#include <inttypes.h>"
      , codeIf (M.fold (\_ e ans -> isMathHCall e || ans ) False (snd mp))
               "#include <math.h>"
      , ""
        -- preamble code supplied by user
      , preCode
      , ""
        -- global clock variable declaration
      , "static " ++ globalType ++ " " ++ globalClk ++ " = 0;"
      , ""
        -- global phase start variable declaration
      , case hardwareClock config of
          Nothing -> ""
          Just _  -> "static " ++ globalType ++ " " ++ phaseStartTime ++ ";"
      , ""
        -- global rule coverage variable declarations
      , codeIf (cRuleCoverage config) $ "static const " ++ cType Word32
                   ++ " __coverage_len = " ++ show covLen ++ ";"
      , codeIf (cRuleCoverage config) $ "static " ++ cType Word32
                   ++ " __coverage[" ++ show covLen ++ "] = {"
                   ++ intercalate ", " (replicate covLen "0") ++ "};"
      , codeIf (cRuleCoverage config)
               ("static " ++ cType Word32 ++ " __coverage_index = 0;")

        -- global state struct declaration
      , declState True (StateHierarchy (cStateName config) [state])

        -- generate functions for each rule
      , concatMap (codeRule mp config) rules'

        -- generate assertion checks
      , codeAssertionChecks mp config assertNames coverNames rules

        -- generate "main" (at least as far as LIMA is concerned)
      , "void " ++ funcName ++ "()"
      , "{"
      , unlines [ swOrHwClock
                , codePeriodPhases
                , "  " ++ globalClk ++ " = " ++ globalClk ++ " + 1;"
                ]
      , "}"
      , ""
      , postCode
      ]

    -- | Generate period/phase scheduling code
    codePeriodPhases :: String
    codePeriodPhases = concatMap (codePeriodPhase config) schedule'

    -- Generate code for handling a hardware clock if needed
    swOrHwClock =
      case hardwareClock config of
        Nothing      -> ""
        Just clkData -> unlines
          [ ""
          , "  " ++ declareConst phaseConst clkDelta
          , "  " ++ declareConst maxConst   maxVal
          , "  static " ++ globalType ++ " " ++ lastPhaseStartTime ++ ";"
          , "  static " ++ globalType ++ " " ++ lastTime ++ ";"
          , "  static bool __first_call = true;"
          , "  " ++ globalType ++ " " ++ currentTime ++ ";"
          , ""
          , "  /* save the current time */"
          , "  " ++ setTime
          , ""
          , "  /* initialize static variables on the first call */"
          , "  if ( __first_call ) {"
          , "    " ++ lastPhaseStartTime ++ " = " ++ phaseStartTime ++ ";"
          , "    " ++ lastTime ++ " = " ++ currentTime ++ ";"
          , "    __first_call = false;"
          , "  }"
          , ""
          , "  /* wait for the amount left for the phase start time to be reached,"
          , "     handle roll-overs of the system timer and the phase start time */"
          , "  if ( " ++ phaseStartTime ++ " >= " ++ lastPhaseStartTime ++ " ) {"
          , "    /* phase start time did not roll over */"
          , "    if ( " ++ currentTime ++ " >= " ++ lastTime ++ " ) {"
          , "      /* system time and the phase start time did not roll over */"
          , "      if ( " ++ phaseStartTime ++ " >= " ++ currentTime ++ " ) {"
          , "        " ++ delayFn ++ " ( " ++ phaseStartTime ++ " - " ++ currentTime ++ " );"
          , "      } else {"
          , "        /* we are late */"
          , "        " ++ errHandler
          , "      }"
          , "    } else {"
          , "      /* system time rolled over, the start time of the"
          , "         phase did not, i.e. we are not late if currentTime"
          , "         is already in between lastPhaseStartTime and phaseStartTime */"
          , "      if ( ( " ++ currentTime ++ " >= " ++ lastPhaseStartTime ++ " )"
          , "             && ( " ++ phaseStartTime ++ " >= " ++ currentTime ++ " ) ) {"
          , "        " ++ delayFn ++ " ( " ++ phaseStartTime ++ " - " ++ currentTime ++ " );"
          , "      } else {"
          , "        /* we are late */"
          , "        " ++ errHandler
          , "      }"
          , "    }"
          , "  } else {"
          , "    /* phase start time rolled over */"
          , "    if ( " ++ currentTime ++ " >= " ++ lastTime ++ " ) {"
          , "      /* current time did not yet roll over */"
          , "      if ( " ++ currentTime ++ " >= " ++ phaseStartTime ++ " ) {"
          , "        " ++ delayFn ++ " ( ( " ++ maxConst
                           ++ " - ( " ++ currentTime
                               ++ " - " ++ phaseStartTime ++ " ) + 1 )" ++ " );"
          , "      } else {"
          , "        /* this should not happen, since " ++ phaseConst ++ " should be"
          , "           smaller than " ++ maxConst ++ " and " ++ lastTime ++ " should"
          , "           be smaller than or equal to " ++ currentTime ++ " */"
          , "        " ++ errHandler
          , "      }"
          , "    } else {"
          , "      /* current time and phase start time rolled over"
          , "         equal to the first case */"
          , "      if ( " ++ phaseStartTime ++ " >= " ++ currentTime ++ " ) {"
          , "        " ++ delayFn ++ " ( " ++ phaseStartTime ++ " - " ++ currentTime ++ " );"
          , "      } else {"
          , "        /* we are late */"
          , "        " ++ errHandler
          , "      }"
          , "    }"
          , "  }"
          , ""
          , ""
          , "  /* update to the next phase start time */"
          , "  " ++ lastPhaseStartTime ++ " = " ++ phaseStartTime ++ ";"
          , "  " ++ phaseStartTime ++ " = " ++ phaseStartTime ++ " + "
                 ++ phaseConst ++ ";"
          , "  " ++ lastTime ++ " = " ++ currentTime ++ ";"
          ]
          where
            delayFn = delay clkData
            maxVal :: Integer
            maxVal  = case clockType clkData of
                        Word8  -> toInteger (maxBound :: Word8)
                        Word16 -> toInteger (maxBound :: Word16)
                        Word32 -> toInteger (maxBound :: Word32)
                        Word64 -> toInteger (maxBound :: Word64)
                        _      -> clkTypeErr
            declareConst varName c' = globalType ++ " const " ++ varName
                                     ++ " = " ++ showConst (constType c') ++ ";"
            setTime     = currentTime ++ " = " ++ clockName clkData ++ "();"
            maxConst    = "__max"
            phaseConst  = "__phase_len"
            currentTime = "__curr_time"
            lastTime    = "__last_time"
            clkDelta | d <= 0
                         = error $ "The delta "
                                   ++ show d
                                   ++ ", given for the number of ticks "
                                   ++ "in a phase must be greater than 0."
                     | d > maxVal
                         = error $ "The delta "
                           ++ show d
                           ++ ", given for the number of ticks in a phase "
                           ++ "must be smaller than "
                           ++ show ( maxVal + 1 )
                           ++ "."
                     | otherwise
                         = d
              where d = delta clkData
            errHandler =
              case err clkData of
                Nothing    -> ""
                Just errF  -> errF ++ " ();"
            constType :: Integer -> Const
            constType c' = case clockType clkData of
                            Word8  -> CWord8  (fromInteger c' :: Word8)
                            Word16 -> CWord16 (fromInteger c' :: Word16)
                            Word32 -> CWord32 (fromInteger c' :: Word32)
                            Word64 -> CWord64 (fromInteger c' :: Word64)
                            _      -> clkTypeErr

    -- generate header file code for the main .c module
    h = unlines
      [ "#include <stdbool.h>"   -- for bool type
      , "#include <stdint.h>"    -- for fixed width integer & unsigned
      , "#include <inttypes.h>"  -- for printf macros
      , ""
      , preHCode
      , ""
      , "void " ++ funcName ++ "();"
      , ""
      , declState False (StateHierarchy (cStateName config) [state])
      , ""
      , postHCode
      ]

    globalType = cType (case hardwareClock config of
                          Nothing      -> Word64 -- Default type
                          Just clkData -> case clockType clkData of
                                            Word8  -> Word8
                                            Word16 -> Word16
                                            Word32 -> Word32
                                            Word64 -> Word64
                                            _      -> clkTypeErr)
    clkTypeErr :: a
    clkTypeErr = error "Clock type must be one of Word8, Word16, Word32, Word64."

    funcName = if null (cFuncName config) then name else cFuncName config

    rules' :: [Rule]
    rules' = concat [ r | (_, _, r) <- schedule' ]

    covLen = 1 + div (maximum $ map ruleId rules') 32

    phaseStartTime     = "__phase_start_time"
    lastPhaseStartTime = "__last_phase_start_time"


-- | Optionally render the given code string
codeIf :: Bool -> String -> String
codeIf a b = if a then b else ""

-- | Generate a C declaration of the global state struct
declState :: Bool -> StateHierarchy -> String
declState define a' = if isHierarchyEmpty a' then ""
  else
     (if define then "" else "extern ") ++ init (init (f1 "" a'))
  ++ (if define then " =\n" ++ f2 "" a' else "") ++ ";\n"
  where
  -- render the declaration section
  -- i :: indentation string
  -- a :: StateHierarchy
  f1 i a = case a of
    StateHierarchy name items ->
         i ++ "struct {  /* " ++ name ++ " */\n"
      ++ concatMap (f1 ("  " ++ i)) items ++ i ++ "} " ++ name ++ ";\n"
    StateVariable  name c     -> i ++ cType (E.typeOf c) ++ " " ++ name ++ ";\n"
    StateArray     name c     ->
         i ++ cType (E.typeOf $ head c) ++ " " ++ name ++ "[" ++ show (length c)
      ++ "];\n"
    -- render channel value and channel ready flag declarations
    StateChannel   name t     ->
         i ++ cType t ++ " " ++ chanVarCName name ++ ";\n"
      ++ i ++ cType Bool ++ " " ++ chanReadyVarCName name
      ++ ";\n"

  -- render the initialization section
  -- i :: indentation string
  -- a :: StateHierarchy
  f2 i a = case a of
    StateHierarchy name items ->
         i ++ "{  /* " ++ name ++ " */\n"
      ++ intercalate ",\n" (map (f2 ("  " ++ i)) items) ++ "\n" ++ i ++ "}"
    StateVariable name c -> i ++ "/* " ++ name ++ " */  " ++ showConst c
    StateArray name c ->
         i ++ "/* " ++ name ++ " */\n" ++ i ++ "{ "
      ++ intercalate ("\n" ++ i ++ ", ") (map showConst c) ++ "\n" ++ i ++ "}"
    -- render channel value and channel ready flag initial values
    StateChannel name t ->
         i ++ "/* " ++ chanVarCName name ++ " */  " ++ initForType t ++ ",\n"
      ++ i ++ "/* " ++ chanReadyVarCName name ++ " */ 0"

-- | Generate C code for a rule as a void/void function @__rN@, where @N@ is
-- the internal rule ID.
--
-- TODO refactor in a pretty printer style; correct indentation level is a
--      pain to keep track of
-- TODO there is a bug here where some code actions get rendered in some
--      contexts but not in others. (see atom-smp recv1 code vs. recv2 code)
codeRule :: UeMap -> Config -> Rule -> String
codeRule mp cfg rule@Rule{} =
    -- function decl
    "/* " ++ show rule ++ " */\n" ++
    "static void __r" ++ show (ruleId rule) ++ "() {\n" ++

    -- declare local vars
    concatMap (codeUE mp cfg ues "  ") ues ++

    -- check inherited and non-inherited enable conditions
    "  if (" ++ id' (ruleEnable rule) ++
                codeEnableNH ++ ") {\n" ++

    -- render all the rule actions
    concatMap codeAction (ruleActions rule) ++

    -- render the rule coverage
    codeIf (cRuleCoverage cfg)
           ( "    __coverage[" ++ covWord ++ "] = __coverage[" ++ covWord
            ++ "] | (1 << " ++ covBit ++ ");\n") ++

    -- render channel writes. Note: channel delay is ignored intentionally,
    -- as it is only supported in the transition system backend.
    concatMap handleChanWrite (ruleChanWrite rule) ++

    -- render channel consumes
    concatMap
      (\cout     -> "    " ++ stateChanReadyVarCName cfg (chanName cout)
                           ++ " = false;\n"
                 ++ "    " ++ stateChanVarCName cfg (chanName cout)
                           ++ " = " ++ initForType (chanType cout) ++ ";\n")
      (ruleChanRead rule) ++

    -- END enable condition
    "  }\n" ++

    -- Render atom assignments
    --
    -- Note: these assignments should occur outside the if block above. They are
    -- typically assignments whose RHS is an expression depending explicitly
    -- on the enable flag.
    concatMap codeAssign (ruleAssigns rule) ++

    -- END rule function
    "}\n\n"
  where
    ues     = map (second showTopo) . topo mp $ allUEs rule
    lkErr u = "in codeRule: failed to lookup untyped expr " ++ show u
    id' ue' = fromMaybe (error $ lkErr ue') $ lookup ue' ues

    codeAction :: ([String] -> String, [Hash]) -> String
    codeAction (f, args) = "      " ++ f (map id' args) ++ ";\n"

    covWord = show $ div (ruleId rule) 32
    covBit  = show $ mod (ruleId rule) 32

    -- Generate C code for effectfull actions: variable assignments, channel
    -- reads/writes, etc.
    codeAssign :: (MUV, Hash) -> String
    codeAssign (uv', ue') = concat ["  ", lh, " = ", id' ue', ";\n"]
      where
      lh = case uv' of
        MUV _ n _                   -> concat [cStateName cfg, ".", n]
        MUVArray (UA _ n _)     idx ->
          concat [cStateName cfg, ".", n, "[", id' idx, "]"]
        MUVArray (UAExtern n _) idx -> concat [n, "[", id' idx, "]"]
        MUVExtern n _               -> n
        MUVChannel{}                ->
          error "MUVChannel can't appear in lhs of assign"
        MUVChannelReady{}           ->
          error "MUVChannelReady can't appear in lhs of assign"

    -- skip rendering the conjunction with 'ruleEnableNH' if it is the same as
    -- ruleEnable (a very common case)
    codeEnableNH = let renh = id' (ruleEnableNH rule)
                   in codeIf (id' (ruleEnable rule) /= renh) (" && " ++ renh)

    handleChanWrite (cin, h, DelayDefault) =
      let cn = chanName cin
      in    "    " ++ stateChanVarCName cfg cn ++ " = " ++ id' h ++ ";\n"
         ++ "    " ++ stateChanReadyVarCName cfg cn ++ " = true;\n"
    -- TODO: for now we just place the message for delivery when
    -- writeChannelWithDelay is called. No delay is introduced.
    handleChanWrite (cin, h, DelayTicks _) =
      trace (unlines [ "WARNING: writeChannelWithDelay with non-default delay"
                     , "is not fully supported\n in the C code generator.\n"
                     ]) $
        let cn = chanName cin
        in    "    " ++ stateChanVarCName cfg cn ++ " = " ++ id' h ++ ";\n"
           ++ "    " ++ stateChanReadyVarCName cfg cn ++ " = true;\n"

-- Don't generate code for the 'Assert' or 'Cover' variants
codeRule _ _ _ = ""

-- | Helpers for generating channel related variables names in the state
-- struct
stateChanVarCName :: Config -> String -> String
stateChanVarCName cfg n = cStateName cfg ++ "." ++ n

stateChanReadyVarCName :: Config -> String -> String
stateChanReadyVarCName cfg n = stateChanVarCName cfg n ++ channelReadySuffix

-- | Variants of above without the @state.@ prefix
chanVarCName :: String -> String
chanVarCName = id

chanReadyVarCName :: String -> String
chanReadyVarCName n = chanVarCName n ++ channelReadySuffix

-- | State struct ready flag suffix for channels
channelReadySuffix :: String
channelReadySuffix = "_ready"

-- | Global clock identifier
globalClk :: String
globalClk = "__global_clock"

-- | Generate C assertions for each LIMA assertion
codeAssertionChecks :: UeMap -> Config -> [Name] -> [Name] -> [Rule] -> String
codeAssertionChecks mp config assertNames coverNames rules =
  codeIf (cAssert config) $
  "static void __assertion_checks() {\n" ++
  concatMap (codeUE mp config ues "  ") ues ++
  concat [     "  if (" ++ id' enable ++ ") " ++ cAssertName config
            ++ "(" ++ assertionId name ++ ", " ++ id' check ++ ", "
            ++ globalClk ++ ");\n"
          | Assert name enable _ check <- rules ] ++
  concat [     "  if (" ++ id' enable ++ ") " ++ cCoverName  config
            ++ "(" ++ coverageId  name ++ ", " ++ id' check ++ ", "
            ++ globalClk ++ ");\n"
          | Cover  name enable _ check <- rules ] ++
  "}\n\n"
  where
  ues = map (second showTopo) . topo mp $ concat [ [a, a', b] | Assert _ a a' b <- rules ]
                                       ++ concat [ [a, a', b] | Cover _ a a' b <- rules ]
  id' ue' = fromJust $ lookup ue' ues
  assertionId :: Name -> String
  assertionId name = show $ fromJust $ elemIndex name assertNames
  coverageId :: Name -> String
  coverageId name = show $ fromJust $ elemIndex name coverNames

-- | Generate code that schedules calls to rules according to the given period
-- and phase offset
codePeriodPhase :: Config -> (Int, Int, [Rule]) -> String
codePeriodPhase config (period, phase, rules) = unlines
  [ printf "  {"
  , printf "    static %s __scheduling_clock = %i;" (cType clockType') phase
  , printf "    if (__scheduling_clock == 0) {"
  , intercalate "\n" $ map callRule rules
  , printf "      __scheduling_clock = %i;" (period - 1)
  , printf "    }"
  , printf "    else {"
  , printf "      __scheduling_clock = __scheduling_clock - 1;"
  , printf "    }"
  , printf "  }"
  ]
  where
  clockType' | period < 2 ^  (8 :: Word8)  = Word8
             | period < 2 ^ (16 :: Word16) = Word16
             | otherwise                   = Word32
  callRule r = concat ["      ", codeIf (cAssert config) "__assertion_checks(); ", "__r", show (ruleId r), "();  /* ", show r, " */"]

-- | Turn topological order on the hash map elements into ordered identifiers
-- in C.
showTopo :: Int -> String
showTopo i = "__" ++ show i

-- | An initial value for variables of the given type. It turns out that 0
-- works perfectly well for all our current types!
initForType :: Type -> String
initForType = const "0"
