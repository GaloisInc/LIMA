-- |
-- Module: Channel
-- Description: Example design which shows off channels
-- Copyright: (c) 2016 Benjamin Jones
--

module Language.SLIM.C.Example.Channel
  ( compileExample
  , example
  ) where

import Language.SLIM
import Language.SLIM.C

-- | Invoke the SLIM compiler
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

-- | An example design that computes the greatest common divisor.
example :: Atom ()
example = do

  -- External reference to value A.
  let a = word32' "a"

  -- External reference to value B.
  let b = word32' "b"

  -- The external running flag.
  let running = bool' "running"

  -- Setup channel from node A to node B
  (cin, cout) <- channel "A_to_B" Word32

  -- A rule to send value of 'a'
  atom "node_A" $ do
    cond $ value a >. Const 0
    writeChannel cin (value a)

  -- A rule to receive a value from the channel
  atom "node_B" $ do
    cond $ fullChannel cout
    b' <- readChannel cout
    b <== b'

  -- A rule to clear the running flag.
  atom "stop" $ do
    cond $ value a ==. value b
    running <== false

