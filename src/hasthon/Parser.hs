module Hasthon.Parser where

import Text.Parsec 
import qualified Text.Parsec.Prim as PS
import qualified Text.Parsec.Error as PE
import Hasthon.Scanner
 
-- abstract syntax tree
data ABSTree = ABSTree
               deriving (Eq, Show)

-- parse error object
data ParseError = ScannerError ScanError
                | ParsecError PE.ParseError
                  deriving (Show)

-- define it's own parser type just like parsec's Parser for string
-- this is because now we're not really parsing string but tokens
type Parser = Parsec [Token] ()


-- test parser to print syntax tree structure
test :: String -> IO ()
test input = do
  case (Hasthon.Parser.parse input) of
      Right abs   -> (putStrLn . show) abs
      Left err    -> print err

-- parse start here
parse :: String -> Either Hasthon.Parser.ParseError ABSTree
parse input = 
   case (scan input) of
      Right tokens   -> case (runParser pythonSyntax () "" tokens) of
                           Right abs   -> Right abs
                           Left err    -> Left $ ParsecError err
      Left err       -> Left $ ScannerError err 

-- root grammar of python syntax
pythonSyntax :: Parser ABSTree
pythonSyntax = do 
   return ABSTree
