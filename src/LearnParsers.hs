module LearnParsers where

import Text.Trifecta
import Control.Applicative

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

data NumberOrString = NOSS String | NOSI Integer
    deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
    deriving (Eq, Show)

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> integer) <|> (NOSS <$> some letter)

parseMeta :: Parser [NumberOrString]
parseMeta = do
  skipDelimiter
  x <- parseNOS
  skipDelimiter
  y <- parseNOS
  return [x, y]

skipDelimiter :: Parser ()
skipDelimiter = skipMany (char '.' <|> char '-')

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  skipDelimiter
  minor <- integer
  skipDelimiter
  patch <- integer
  release <- parseMeta
  return $ SemVer major minor patch release []