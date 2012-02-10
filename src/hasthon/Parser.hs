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

-- parse mode
data ParseMode = SingleInput
               | FileInput
               | EvalInput
               deriving (Eq, Show)


-- test parser to print syntax tree structure
test :: ParseMode -> String -> IO ()
test mode input = do
  case (Hasthon.Parser.parse mode input) of
      Right abs   -> (putStrLn . show) abs
      Left err    -> print err

-- parse start here
parse :: ParseMode -> String -> Either Hasthon.Parser.ParseError ABSTree
parse mode input = 
   case (scan input) of
      Right tokens   -> case (runParser pythonGrammar () "" tokens) of
                           Right abs   -> Right abs
                           Left err    -> Left $ ParsecError err
      Left err       -> Left $ ScannerError err 
   where pythonGrammar = case mode of
                            SingleInput -> singleInputGrammar
                            FileInput   -> fileInputGrammar
                            EvalInput   -> evalInputGrammar

-- single input grammar
singleInputGrammar :: Parser ABSTree
singleInputGrammar = do 
   fail ""
   return ABSTree

-- file input grammar
fileInputGrammar :: Parser ABSTree
fileInputGrammar = do 
   many stmt
   return ABSTree

-- eval input grammar
evalInputGrammar :: Parser ABSTree
evalInputGrammar = do 
   fail ""
   return ABSTree

-- statement
stmt :: Parser ABSTree
stmt = simpleStmt <|> compoundStmt

-- simple statement
simpleStmt :: Parser ABSTree
simpleStmt = do
   firstStmt <- smallStmt
   otherStmts <- many (pDEL ";" >> smallStmt >>= return)
   optional $ pDEL ";"
   pNEWLINE
   return ABSTree

-- compound statement statement
compoundStmt :: Parser ABSTree
compoundStmt = do
   fail ""
   return ABSTree

-- small statement
smallStmt :: Parser ABSTree
smallStmt = do
   pKW "for"
   return ABSTree








-- utility function to help parsing token
tok :: Token -> Parser Token
tok t@(Token typ _) = tokenPrim showTok nextPos testTok
   where showTok t'                 = show t'
         nextPos p t' ts            = incSourceColumn p 1
         testTok t'@(Token typ' _)  = if tokcmp typ typ' then Just t' else Nothing

toks :: [Token] -> Parser [Token]
toks = tokens showTok nextPos 
   where showTok ts'   = show ts'
         nextPos p ts' = incSourceColumn p $ length ts'

-- utility to help comparing token type but not its actual value for some type
tokcmp (TTIndent      _)               (TTIndent _)                       = True
tokcmp (TTIndent      _)               _                                  = False
tokcmp (TTDedent      _)               (TTDedent _)                       = True
tokcmp (TTDedent      _)               _                                  = False
tokcmp (TTIdentifier  _)               (TTIdentifier _)                   = True
tokcmp (TTIdentifier  _)               _                                  = False
tokcmp (TTLiteral (LTString       _))  (TTLiteral (LTString      _))      = True
tokcmp (TTLiteral (LTBytes        _))  (TTLiteral (LTBytes       _))      = True
tokcmp (TTLiteral (LTInteger      _))  (TTLiteral (LTInteger     _))      = True
tokcmp (TTLiteral (LTFloat        _))  (TTLiteral (LTFloat       _))      = True
tokcmp (TTLiteral (LTImaginary  _ _))  (TTLiteral (LTImaginary _ _))      = True
tokcmp (TTLiteral _                 )  (TTLiteral _                )      = False
tokcmp t1                           t2                                    = t1 == t2

-- utility to make a token only for the part we care about
mktok :: TokenType -> Token
mktok tt = (Token tt (0,0))

-- various token parsers
pINDENT        = tok $ mktok $ TTIndent 0
pDEDENT        = tok $ mktok $ TTDedent 0
pNEWLINE       = tok $ mktok TTNewline
pKW s          = tok $ mktok $ TTKeyword s
pID            = tok $ mktok $ TTIdentifier ""
pLTSTR         = tok $ mktok $ TTLiteral $ LTString ""
pLTBYTES       = tok $ mktok $ TTLiteral $ LTBytes ""
pLTINTEGER     = tok $ mktok $ TTLiteral $ LTInteger ""
pLTFLOAT       = tok $ mktok $ TTLiteral $ LTFloat ""
pLTIMAGINARY   = tok $ mktok $ TTLiteral $ LTImaginary "" ""
pOP s          = tok $ mktok $ TTOperator s
pDEL s         = tok $ mktok $ TTDelimeter s



