module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1' >> eof

one23 :: Parser String
one23 = string "1"

-- read a single character '1', then die
one' :: Parser b
one' = one >> stop

oneTwo = char '1' >> char '2' >> eof

oneTwo' = oneTwo >> stop

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

p123 :: Show a => Parser a -> IO ()
p123 p = print $ parseString p mempty "123"