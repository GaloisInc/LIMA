-- | 
-- Module: Language
-- Description: Definitions for the language/EDSL itself
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Definitions for the LIMA EDSL itself

module Language.LIMA.Language
  (
    module Language.LIMA.Expressions
  , module Language.LIMA.Channel
  , module Language.LIMA.Channel.Types
  -- * Primary Language Containers
  , Atom
  -- * Compilation parameters
  , CompCtx (..)
  , defCCtx
  , defSCtx
  -- * Hierarchical Rule Declarations
  , atom
  , getNewClock
  , getName
  , getCompiledName
  , period
  , getPeriod
  , phase
  , exactPhase
  , getPhase
  -- * Action Directives
  , cond
  , cond'
  , Assign (..)
  , incr
  , decr
  -- * Variable Declarations
  , var
  , var'
  , array
  , array'
  , bool
  , bool'
  , int8
  , int8'
  , int16
  , int16'
  , int32
  , int32'
  , int64
  , int64'
  , word8
  , word8'
  , word16
  , word16'
  , word32
  , word32'
  , word64
  , word64'
  , float
  , float'
  , double
  , double'
  -- * Custom Actions
  , action
  , call
  -- * Probing
  , probe
  , probes
  -- * Assertions and Functional Coverage
  , assert
  , cover
  , assertImply
  -- * Utilities
  , Name
  , path
  , clock
  -- * Code Coverage
  , nextCoverage
  -- * Rewriting
  -- , rewriteAtom
  ) where

import Data.Int
import Data.Word
import Data.List (foldl')

import MonadLib

import Language.LIMA.Channel
import Language.LIMA.Channel.Types
import Language.LIMA.Elaboration hiding (Atom)
import qualified Language.LIMA.Elaboration as E
import Language.LIMA.Expressions
import Language.LIMA.UeMap hiding (typeOf)
import Language.LIMA.Types

infixr 1 <==

-- | The Atom monad captures variable and transition rule declarations.
type Atom = E.Atom

-- | Creates a hierarchical node, where each node could be an atomic rule.
atom :: Name -> Atom a -> Atom a
atom name design = do
  name' <- addName name
  (st1, (g1, parent)) <- get
  ctx <- ask
  let ((a, nts), atst) = buildAtom ctx st1 g1 { gState = [] } name' design
      (st2, (g2, child)) = atst
  set (st2, ( g2 { gState = gState g1 ++ [StateHierarchy name $ gState g2] }
            , parent { atomSubs = atomSubs parent ++ [child] }))
  put (reverse nts)
  return a

-- | Return the next available clock Id
getNewClock :: Atom Int
getNewClock = do
  (st, (g, a)) <- get
  let clkId = gClockId g
  set (st, (g { gClockId = clkId+1 }, a))
  return clkId

-- | Return the top-level name of the atom.
getName :: Atom Name
getName = do
  (_st, (_g, a)) <- get
  return (atomName a)

-- | Get the "most-unique" name of an atom in a non-atom context.
getCompiledName :: Atom a -> Name
getCompiledName atm =
  let ((_, _nts), atst) = buildAtom defCCtx emptyMap initialGlobal "" atm
      (_u, (_g, db)) = atst
  in case atomSubs db of
    [d] -> atomName d   -- unique subatom found
    _   -> atomName db  -- no subatoms found

-- | Defines the period of execution of sub-rules as a factor of the base rate
-- of the system.  Rule period is bound by the closest period assertion.  For
-- example:
--
-- > period 10 $ period 2 a   -- Rules in 'a' have a period of 2, not 10.
period :: Int -> Atom a -> Atom a
period n _ | n <= 0 = error "ERROR: Execution period must be greater than 0."
period n atom' = do
  (st, (g, a)) <- get
  set (st, (g { gPeriod = n }, a))
  r <- atom'
  (st', (g', a')) <- get
  set (st', (g' { gPeriod = gPeriod g }, a'))
  return r

-- | Returns the execution period of the current scope.
getPeriod :: Atom Int
getPeriod = do
  (_, (g, _)) <- get
  return $ gPeriod g

phase' :: (Int -> Phase) -> Int -> Atom a -> Atom a
phase' _ n _ | n < 0 = error $ "ERROR: phase " ++ show n ++ " must be at least 0."
phase' phType n atom' = do
  (st, (g, a)) <- get
  if n >= gPeriod g
    then error $ "ERROR: phase " ++ show n ++ " must be less than the current period "
               ++ show (gPeriod g) ++ "."
    else do set (st, (g { gPhase = phType n }, a))
            r <- atom'
            (st', (g', a')) <- get
            set (st', (g' { gPhase = gPhase g }, a'))
            return r

-- | Defines the earliest phase within the period at which the rule should
-- execute; the scheduler attempt to find an optimal phase from 0 <= @n@ <
-- period (thus, the 'phase' must be at least zero and less than the current
-- 'period').
phase :: Int -> Atom a -> Atom a
phase = phase' MinPhase

-- | Ensures an atom is scheduled only at phase @n@.
exactPhase :: Int -> Atom a -> Atom a
exactPhase = phase' ExactPhase

-- | Returns the phase of the current scope.
getPhase :: Atom Int
getPhase = do
  (_, (g, _)) <- get
  return $ case gPhase g of
             MinPhase ph   -> ph
             ExactPhase ph -> ph

-- | Returns the current atom hierarchical path.
path :: Atom String
path = do
  (_, (_, atom')) <- get
  return $ atomName atom'

-- | Local boolean variable declaration.
bool :: Name -> Bool -> Atom (V Bool)
bool = var

-- | External boolean variable declaration.
bool' :: Name -> V Bool
bool' name = var' name Bool

-- | Local int8 variable declaration.
int8 :: Name -> Int8 -> Atom (V Int8)
int8 = var

-- | External int8 variable declaration.
int8' :: Name -> V Int8
int8' name = var' name Int8

-- | Local int16 variable declaration.
int16 :: Name -> Int16 -> Atom (V Int16)
int16 = var

-- | External int16 variable declaration.
int16' :: Name -> V Int16
int16' name = var' name Int16

-- | Local int32 variable declaration.
int32 :: Name -> Int32 -> Atom (V Int32)
int32 = var

-- | External int32 variable declaration.
int32' :: Name -> V Int32
int32' name = var' name Int32

-- | Local int64 variable declaration.
int64 :: Name -> Int64 -> Atom (V Int64)
int64 = var

-- | External int64 variable declaration.
int64' :: Name -> V Int64
int64' name = var' name Int64

-- | Local word8 variable declaration.
word8 :: Name -> Word8 -> Atom (V Word8)
word8 = var

-- | External word8 variable declaration.
word8' :: Name -> V Word8
word8' name = var' name Word8

-- | Local word16 variable declaration.
word16 :: Name -> Word16 -> Atom (V Word16)
word16 = var

-- | External word16 variable declaration.
word16' :: Name -> V Word16
word16' name = var' name Word16

-- | Local word32 variable declaration.
word32 :: Name -> Word32 -> Atom (V Word32)
word32 = var

-- | External word32 variable declaration.
word32' :: Name -> V Word32
word32' name = var' name Word32

-- | Local word64 variable declaration.
word64 :: Name -> Word64 -> Atom (V Word64)
word64 = var

-- | External word64 variable declaration.
word64' :: Name -> V Word64
word64' name = var' name Word64

-- | Local float variable declaration.
float :: Name -> Float -> Atom (V Float)
float = var

-- | External float variable declaration.
float' :: Name -> V Float
float' name = var' name Float

-- | Local double variable declaration.
double :: Name -> Double -> Atom (V Double)
double = var

-- | External double variable declaration.
double' :: Name -> V Double
double' name = var' name Double

-- | Declares an action, which executes C code that is optionally passed
-- some parameters.
action :: ([String] -> String) -- ^ A function which receives a list of
                               -- C parameters, and returns C code that
                               -- should be executed.
          -> [UE] -- ^ A list of expressions; the supplied functions receive
                  -- parameters which correspond to these expressions.
          -> Atom ()
action f ues = do
  (st, (g, a)) <- get
  let (st', hashes) =
        foldl' (\(accSt,hs) ue' ->
                 let (h,accSt') = newUE ue' accSt in (accSt',h:hs))
        (st,[]) ues
  set (st', (g, a { atomActions = atomActions a ++ [(f, hashes)] }))

-- | Calls an external C function of type 'void f(void)'.
call :: Name -- ^ Function @f@
        -> Atom ()
call n = action (\ _ -> n ++ "()") []

-- | Declares a probe. A probe allows inspecting any expression, remotely to
-- its context, at any desired rate.
probe :: Expr a => Name -- ^ Human-readable probe name
         -> E a -- ^ Expression to inspect
         -> Atom ()
probe name a = do
  (st, (g, atom')) <- get
  let (h,st') = newUE (ue a) st
  if any (\ (n, _) -> name == n) $ gProbes g
    then error $ "ERROR: Duplicated probe name: " ++ name
    else set (st', (g { gProbes = (name, h) : gProbes g }, atom'))

-- | Fetches all declared probes to current design point.  The list contained
-- therein is (probe name, untyped expression).
-- See 'Language.LIMA.Unit.printProbe'.
probes :: Atom [(String, UE)]
probes = do
  (st, (g, _)) <- get
  let (strs,hs) = unzip (gProbes g)
  let g' = zip strs (map (recoverUE st) hs)
  return g'

-- | Increments a 'NumE' 'V'.
incr :: (Assign a, NumE a) => V a -> Atom ()
incr a = a <== value a + 1

-- | Decrements a 'NumE' 'V'.
decr :: (Assign a, NumE a) => V a -> Atom ()
decr a = a <== value a - 1


class Expr a => Assign a where
  -- | Assign an 'E' to a 'V'.
  (<==) :: V a -> E a -> Atom ()
  v <== e = do
    (st, (g, atom')) <- get
    let (h,st0) = newUE (ue e) st
    let (muv,st1) = newUV (uv v) st0
    set (st1, (g, atom' { atomAssigns = (muv, h) : atomAssigns atom' }))

instance Assign Bool
instance Assign Int8
instance Assign Int16
instance Assign Int32
instance Assign Int64
instance Assign Word8
instance Assign Word16
instance Assign Word32
instance Assign Word64
instance Assign Float
instance Assign Double

-- | Adds an enabling condition to an atom subtree of rules.
-- This condition must be true before any rules in hierarchy
-- are allowed to execute.
cond :: E Bool -> Atom ()
cond c = do
  (st, (g, atom')) <- get
  let ae = recoverUE st (atomEnable atom')
  let (h, st') = newUE (uand ae (ue c)) st
  set (st', (g, atom' { atomEnable = h }))

-- | Similar to 'cond', but does not inherit the enable condition from its
-- parent.
cond' :: E Bool -> Atom ()
cond' c = do
  (st, (g, atom')) <- get
  let (h, st') = newUE (ue c) st  -- no inheritance
  set (st', (g, atom' { atomEnableNH = h }))

-- | Reference to the 64-bit free running clock.
clock :: E Word64
clock = value $ word64' "__global_clock"

-- | Rule coverage information.  (current coverage index, coverage data)
nextCoverage :: Atom (E Word32, E Word32)
nextCoverage = do
  action (const "__coverage_index = (__coverage_index + 1) % __coverage_len") []
  return (value $ word32' "__coverage_index", value $ word32' "__coverage[__coverage_index]")

-- | An assertions checks that an 'E Bool' is true.  Assertions are checked
-- between the execution of every rule.  Parent enabling conditions can
-- disable assertions, but period and phase constraints do not.  Assertion
-- names should be globally unique.
assert :: Name -> E Bool -> Atom ()
assert name check = do
  (st, (g, atom')) <- get
  -- TODO
  -- let names = map fst (atomAsserts atom')
  -- when (name `elem` names)
  --      (liftIO $ putStrLn $ "WARNING: Assertion name already used: " ++ name)
  let (chk,st') = newUE (ue check) st
  set (st', (g, atom' { atomAsserts = (name, chk) : atomAsserts atom' }))

-- | Implication assertions.  Creates an implicit coverage point for the
-- precondition.
assertImply :: Name -> E Bool -> E Bool -> Atom ()
assertImply name a b = do
  assert name $ imply a b
  cover (name ++ "Precondition") a

-- | A functional coverage point tracks if an event has occurred (true).
-- Coverage points are checked at the same time as assertions.
-- Coverage names should be globally unique.
cover :: Name -> E Bool -> Atom ()
cover name check = do
  (st, (g, atom')) <- get
  -- TODO
  -- let names = map fst (atomCovers atom')
  -- when (name `elem` names)
  --      (liftIO . putStrLn $ "WARNING: Coverage name already used: " ++ name)
  let (chk,st') = newUE (ue check) st
  set (st', (g, atom' { atomCovers = (name, chk) : atomCovers atom' }))

-- | Recursive bottom-up rewrite an atom.
-- rewriteAtom :: (Atom () -> Atom ()) -> Atom () -> Atom ()
-- rewriteAtom rw atm = rw atm'
--   where
--     atm' = do
--       -- Run the atom monad's state computation, getting back the uemap,
--       -- globals, and atom database.
--       let (_, (u, (g, db))) = buildAtom emptyMap initialGlobal "foo" atm
--           subs = atomSubs db
--       -- Define a fold function that applies 'rewriteAtom' recursively
--       -- down the subatom hierarchy, keeping an updated uemap and global
--       -- data as it goes.
--           f :: (UeMap, Global, [AtomDB]) -> AtomDB -> (UeMap, Global, [AtomDB])
--           f (u0, g0, adbs) adb =
--             let a = set (u0, (g0, adb))  -- make an atom of adb
--                 a' = rewriteAtom rw a    -- rewrite the atom using u, g
--                 (_, (u0', (g0', adb'))) = buildAtom u0 g0 (atomName adb) a'
--             in trace (show u0 ++ "\n" ++ show g0 ++ "\n") (u0', g0', adbs ++ [adb'])
--       let (u', g', adbs') = foldl' f (u, g, []) subs
--       -- Update top-level database with the rewritten subatoms.
--       let db' = db { atomSubs = adbs' }
--       -- Set the state of the rewritten atom directly.
--       set (u', (g', db'))

--       -- _ <- atm
--       -- (u, (g, db)) <- get
--       -- let subs = atomSubs db
--       -- let f :: (UeMap, Global, [AtomDB]) -> AtomDB -> Atom (UeMap, Global, [AtomDB])
--       --     f (u0, g0, adbs) adb = do
--       --       let a = set (u0, (g0, adb))  -- make an atom of adb
--       --       let a' = rewriteAtom rw a  -- rewrite the atom using u, g
--       --       _ <- a'                    -- run the rewritten atom
--       --       (u0', (g0', adb')) <- get
--       --       return (u0', g0', adbs ++ [adb'])
--       -- (u', g', adbs') <- foldM f (u, g, []) subs
--       -- let db' = db { atomSubs = adbs' }
--       -- set (u', (g', db'))  -- update state of parent
