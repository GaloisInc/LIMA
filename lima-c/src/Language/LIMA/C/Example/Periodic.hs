-- |
-- Module: Periodic
-- Description: Example design which shows off periodic Atoms
-- Copyright: (c) 2017 Benjamin Jones
--

module Language.LIMA.C.Example.Periodic
  ( compileExample
  , example
  ) where

import Language.LIMA
import Language.LIMA.C

-- | Invoke the LIMA compiler
compileExample :: IO ()
compileExample = do
  let cfg = defaults { cCode = prePostCode }
  CompileResult schedule _ _ _ _ _ <- compile "example" cfg example
  putStrLn $ reportSchedule schedule

prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
  ( unlines
    [ "#include <stdlib.h>"
    , "#include <stdio.h>"
    ]
  , unlines
    [ "int main(int argc, char* argv[]) {"
    , "  while (1) {"
    , "    example();"
    , "    usleep(1000);"
    , "  }"
    , "  return 0;"
    , "}"
    ]
  )

-- | An example design that executes multiple tasks with different
-- periodicity.
example :: Atom ()
example = atom "top_level" $ do
  counter <- int64 "top_counter" 0
  probe "counter" (value counter)
  incr counter  -- increments every tick

  period 4 $ atom "period_4_atom" $ do
    counter4 <- int64 "counter4" 0
    probe "counter4" (value counter4)
    incr counter4  -- increments every 4 ticks

    period 2 $ atom "period_4_2_atom" $ do
      counter42 <- int64 "counter_4_2" 0
      probe "counter42" (value counter42)
      incr counter42  -- increments every 2 ticks

    period 4 $ atom "period_4_4_atom" $ do
      counter44 <- int64 "counter_4_4" 0
      probe "counter44" (value counter44)
      incr counter44  -- increments every 4 ticks

  printAllProbes

-- | Print to stdout all probe values
printAllProbes :: Atom ()
printAllProbes = mapM_ printProbe =<< probes
