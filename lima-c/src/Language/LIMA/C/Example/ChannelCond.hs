-- |
-- Module: ChannelCond
-- Description: Example design which shows off the two different channel
--              'cond' primitives
-- Copyright: (c) 2017 Benjamin Jones
--

module Language.LIMA.C.Example.ChannelCond
  ( compileExample
  , example
  ) where

import Language.LIMA
import Language.LIMA.C

-- | Invoke the LIMA compiler
compileExample :: IO ()
compileExample = do
  res <- compile "example" defaults { cCode = prePostCode } example
  putStrLn $ reportSchedule (compSchedule res)

prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
  ( unlines
    [ "#include <stdlib.h>"
    , "#include <stdio.h>"
    , "unsigned long int a = 13;"
    , "unsigned long int b = -1;"
    , "unsigned char running = 1;"
    ]
  , unlines
    [ "int main(int argc, char* argv[]) {"
    , "  printf(\"Sending value a = %lu\\n\", a);"
    , "  while(running) {"
    , "    example();"
    , "    printf(\"iteration:  a = %lu  b = %lu\\n\", a, b);"
    , "  }"
    , "  printf(\"Receiver's result: %lu\\n\", b);"
    , "  return 0;"
    , "}"
    ]
  )

-- | An example design
example :: Atom ()
example = do

  -- External reference to value A.
  let a = word32' "a"

  -- External reference to value B.
  let b = word32' "b"

  -- The external running flag.
  let running = bool' "running"

  subCounter <- int64 "subCounter" 0
  probe "subCounter" (value subCounter)

  -- Setup channel from node A to node B
  (cin, cout) <- channel "A_to_B" Word32

  -- A rule to send value of 'a'
  atom "node_A" $ do
    cond $ value running
    writeChannel cin (value a)

  -- A rule to receive a value from the channel
  atom "node_B" $ do
    cond' $ fullChannel cout
    b' <- readChannel cout
    b <== b'

    -- Sub-atom of node B
    atom "sub_node_B" $ do
      cond $ value running
      incr subCounter

  -- A rule to clear the running flag.
  atom "stop" $ do
    cond $ value a ==. value b
    running <== false

  mapM_ printProbe =<< probes
