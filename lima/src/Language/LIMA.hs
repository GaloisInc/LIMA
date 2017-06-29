{- |
Module: Language.LIMA
Description: Top-level LIMA module
Copyright: (c) 2013 Tom Hawkins & Lee Pike
Copyright: (c) 2017 Benjamin Jones

LIMA is a Haskell DSL for designing hard realtime embedded software.

Based on guarded atomic actions (similar to STM), LIMA enables highly
concurrent programming without the need for mutex locking.
In addition, LIMA performs compile-time task scheduling and generates code
with deterministic execution time and constant memory use, simplifying the
process of timing verification and memory consumption in hard realtime
applications. Without mutex locking and run-time task scheduling,
LIMA eliminates the need and overhead of RTOSes for many embedded applications.
-}

module Language.LIMA
  ( -- * Common
    -- | Module: "Language.LIMA.Common"
    Timer, timer, startTimer, startTimerIf, timerDone, oneShotRise,
    oneShotFall, debounce, lookupTable, linear, hysteresis, clocked,
    mkSWEther, mkStarBus,
    -- ** Signal fading
    -- | Module: "Language.LIMA.Common.Fader"
    Fader, FaderInit (..), fader, fadeToA, fadeToB, fadeToCenter,
    -- ** Thresholds
    -- | Module: "Language.LIMA.Common.Threshold"
    boolThreshold, doubleThreshold,
    -- ** Valid/Invalid data
    -- | Module: "Language.LIMA.Common.ValidData"
    ValidData, validData, getValidData, whenValid, whenInvalid,
    -- * Language & EDSL
    -- | Module: "Language.LIMA.Language"
    Atom, CompCtx(..), defCCtx, defSCtx, atom, getName, period, getPeriod,
    phase, exactPhase, getPhase, cond, cond', Assign (..), incr, decr, var,
    var', array, array', bool, bool', int8, int8', int16, int16', int32,
    int32', int64, int64', word8, word8', word16, word16', word32, word32',
    word64, word64', float, float', double, double', action, call, probe,
    probes, assert, cover, assertImply, Name, path, clock, nextCoverage,
    -- ** channels
    channel, ChanInput (..), ChanOutput (..),
    writeChannelWithDelay, writeChannel, readChannel, initChannel,
    fullChannel, ChannelDelay(..),
    -- * Expressions
    -- | Module: "Language.LIMA.Expressions"
    E(..), V(..), UE(..), UV(..), A(..), UA(..), Expr(..), Expression(..),
    Variable(..), Type(..), Const(..), Width(..), TypeOf(..), bytes, ue, uv,
    NumE, IntegralE, FloatingE, EqE, OrdE, true, false,
    value, not_, (&&.), (||.), and_, or_, any_, all_, imply, (.&.), complement,
    (.|.), xor, (.<<.), (.>>.), rol, ror, bitSize, isSigned, (==.), (/=.),
    (<.), (<=.), (>.), (>=.), min_, minimum_, max_, maximum_, limit, div_,
    div0_, mod_, mod0_, mux, (!), (!.), ubool, unot, uand, uor, ueq, umux,
  ) where

import Language.LIMA.Common
import Language.LIMA.Common.Fader
import Language.LIMA.Common.Threshold
import Language.LIMA.Common.ValidData
import Language.LIMA.Language
