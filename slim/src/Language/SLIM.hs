{- |
Module: Language.SLIM
Description: Top-level SLIM module
Copyright: (c) 2013 Tom Hawkins & Lee Pike
Copyright: (c) 2017 Benjamin Jones

SLIM is a Haskell DSL for designing hard realtime embedded software.

Based on guarded atomic actions (similar to STM), SLIM enables highly
concurrent programming without the need for mutex locking.
In addition, SLIM performs compile-time task scheduling and generates code
with deterministic execution time and constant memory use, simplifying the
process of timing verification and memory consumption in hard realtime
applications. Without mutex locking and run-time task scheduling,
SLIM eliminates the need and overhead of RTOSes for many embedded applications.
-}

module Language.SLIM
  ( -- * Common
    -- | Module: "Language.SLIM.Common"
    Timer, timer, startTimer, startTimerIf, timerDone, oneShotRise,
    oneShotFall, debounce, lookupTable, linear, hysteresis, clocked,
    mkSWEther, mkStarBus,
    -- ** Signal fading
    -- | Module: "Language.SLIM.Common.Fader"
    Fader, FaderInit (..), fader, fadeToA, fadeToB, fadeToCenter,
    -- ** Thresholds
    -- | Module: "Language.SLIM.Common.Threshold"
    boolThreshold, doubleThreshold,
    -- ** Valid/Invalid data
    -- | Module: "Language.SLIM.Common.ValidData"
    ValidData, validData, getValidData, whenValid, whenInvalid,
    -- * Language & EDSL
    -- | Module: "Language.SLIM.Language"
    Atom, atom, period, getPeriod, phase, exactPhase, getPhase, cond, cond',
    Assign (..), incr, decr, var, var', array, array', bool, bool', int8,
    int8', int16, int16', int32, int32', int64, int64', word8, word8', word16,
    word16', word32, word32', word64, word64', float, float', double, double',
    action, call, probe, probes, assert, cover, assertImply, Name,
    path, clock, nextCoverage,
    -- ** channels
    channel, ChanInput (..), ChanOutput (..),
    writeChannelWithDelay, writeChannel,
    readChannel, fullChannel,
    -- * Expressions
    -- | Module: "Language.SLIM.Expressions"
    E(..), V(..), UE(..), UV(..), A(..), UA(..), Expr(..), Expression(..),
    Variable(..), Type(..), Const(..), Width(..), TypeOf(..), bytes, ue, uv,
    NumE, IntegralE, FloatingE, EqE, OrdE, true, false,
    value, not_, (&&.), (||.), and_, or_, any_, all_, imply, (.&.), complement,
    (.|.), xor, (.<<.), (.>>.), rol, ror, bitSize, isSigned, (==.), (/=.),
    (<.), (<=.), (>.), (>=.), min_, minimum_, max_, maximum_, limit, div_,
    div0_, mod_, mod0_, mux, (!), (!.), ubool, unot, uand, uor, ueq, umux,
  ) where

import Language.SLIM.Common
import Language.SLIM.Common.Fader
import Language.SLIM.Common.Threshold
import Language.SLIM.Common.ValidData
import Language.SLIM.Language
