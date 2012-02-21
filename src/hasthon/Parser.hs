module Hasthon.Parser where

import Text.Parsec 
import qualified Text.Parsec.Prim as PS
import qualified Text.Parsec.Error as PE
import Hasthon.Scanner
 
-- abstract syntax tree
data ABSTree = ABSTree String
             | ABSStmt Statement
             | Stmts [ABSTree]
               deriving (Eq, Show)


-- statement
data Statement = STPass 
               | STBreak
               | STContinue
               | STReturn (Maybe [Expression])
               | STRaise (Maybe (Expression, Maybe Expression))
               | STYield (Maybe Expression)
               | STGlobal [Token]
               | STNonlocal [Token]
                 deriving (Eq, Show)

-- expresion
data Expression = EXString String
                | EXTest Expression (Maybe (Expression, Expression))
                | EXOrTest [Expression]
                | EXAndTest [Expression]
                | EXNotTest Expression
                | EXComparison Expression [(Token, Expression)]
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



--test :: ParseMode -> String -> IO ()
--test mode input = do
--  case (Hasthon.Scanner.scan input) of
--      Right tokens   -> mapM_ (putStrLn . show) tokens
--      Left err       -> print err
--

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
   return $ ABSTree "singleInputGrammar"

-- file input grammar
fileInputGrammar :: Parser ABSTree
fileInputGrammar = do 
   stmts <- many stmt
   return $ if null stmts then ABSTree "fileInputGrammar" else head stmts

-- eval input grammar
evalInputGrammar :: Parser ABSTree
evalInputGrammar = do 
   fail ""
   return $ ABSTree "evalInputGrammar"

-- statement
stmt :: Parser ABSTree
stmt = do 
   simpleStmt <|> compoundStmt

-- simple statement
simpleStmt :: Parser ABSTree
simpleStmt = do
   firstStmt <- smallStmt
   otherStmts <- many (pDEL ";" >> smallStmt >>= return)
   optional $ pDEL ";"
   pNEWLINE
   return $ Stmts (firstStmt : otherStmts)

-- compound statement statement
compoundStmt :: Parser ABSTree
compoundStmt = do
   fail ""
   return $ ABSTree "compoundStmt"

-- small statement
smallStmt :: Parser ABSTree
smallStmt = 
   exprStmt <|> delStmt <|> passStmt <|> flowStmt <|>
      importStmt <|> globalStmt <|> nonlocalStmt <|> assertStmt


-- expresion statement
exprStmt :: Parser ABSTree
exprStmt = do
   fail ""
   return $ ABSTree "exprStmt"
   
-- delete statement
delStmt :: Parser ABSTree
delStmt = do
   fail ""
   return $ ABSTree "delStmt"
   
-- pass statement
passStmt :: Parser ABSTree
passStmt = do
   pKW "pass"
   return $ ABSStmt STPass
   
-- flow statement
flowStmt :: Parser ABSTree
flowStmt = breakStmt <|> continueStmt <|> returnStmt <|> raiseStmt <|> yieldStmt

-- import statement
importStmt :: Parser ABSTree
importStmt = do
   fail ""
   return $ ABSTree "importStmt"
   
-- global statement
globalStmt :: Parser ABSTree
globalStmt = do
   pKW "global"
   names <- pID `sepBy1` pDEL ","
   return $ ABSStmt $ STGlobal names
   
-- non-local statement
nonlocalStmt :: Parser ABSTree
nonlocalStmt = do
   pKW "nonlocal"
   names <- pID `sepBy1` pDEL ","
   return $ ABSStmt $ STNonlocal names
   
-- assert statement
assertStmt :: Parser ABSTree
assertStmt = do
   fail ""
   return $ ABSTree "assertStmt"

-- break statment
breakStmt :: Parser ABSTree
breakStmt = do
   pKW "break"
   return $ ABSStmt STBreak

-- continue statment
continueStmt :: Parser ABSTree
continueStmt = do
   pKW "continue"
   return $ ABSStmt STContinue

-- return statement
returnStmt :: Parser ABSTree
returnStmt = do
   pKW "return"
   rTestlist <- optionMaybe pTestlist
   return $ ABSStmt $ STReturn rTestlist

-- raise statement
raiseStmt :: Parser ABSTree
raiseStmt = do
   pKW "raise" 
   rTest <- optionMaybe (do rTest' <- pTest
                            rFrom' <- optionalMaybe (pKW "from" >> pTest)
                            return (rTest', rFrom'))
   return $ ABSStmt $ STRaise rTest

-- yield statement
yieldStmt :: Parser ABSTree
yieldStmt = yieldExpr

-- yield expression
yieldExpr :: Parser ABSTree
yieldExpr = do
   pKW "yield"
   return $ ABSStmt $ STYield Nothing


-- test list epression
pTestlist :: Parser [Expression]
pTestlist = do
   rTests <- pTest `sepBy1` pDEL ","
   optional $ pDEL ","
   return rTests

-- test expression
pTest :: Parser Expression
pTest = do
   (rOrTest <- pOrTest
    rIfCond <- optionalMaybe (do pKW "if" 
                                rCond <- pOrTest
                                pKW "else"
                                rTest <- pTest

                                return (rCond, rTest))
    return $ EXTestExpr rOrTest rIfcond
   ) <|> pLambdef 

-- or-test expression
pOrTest :: Parser Expression
pOrTest = do
   rAndTests <- pAndTest `sepBy1` pKW "or"
   return $ EXOrTest rAndTests

-- and-test expression
pAndTest :: Parser Expression
pAndTest = do
   rNotTests <- pNotTest `sepBy1` pKW "and"
   return $ EXAndtest rNotTests

-- not-test expression
pNotTest :: Parser Expression
pNotTest = do
   rNotTest <- (pKW "not" >> pNotTest) <|> pComparison
   return $ EXNotTest rNotTest

-- comparision expression
pComparison :: Parser Expression
pComparison = do
   rStarExpr <- pStarExpr 
   rCompElements <- many (do rCompOp <- pCompOp
                             rStarExpr' <- pStarExpr
                             return (rCompOp, rStarExpr'))
   return $ EXComparison rStarExpr rCompElements

-- comparison operator 
pCompOp :: Parser Token
pCompOp = 


-- utility function to help parsing token
tok :: Token -> String -> Parser Token
tok t@(Token typ _) msg = tokenPrim showTok nextPos testTok <?> msg
   where showTok t'                                    = show t'
         nextPos _ _               ((Token _ (p,_)):_) = fromSPos p
         nextPos _ (Token _ (_,p)) []                  = fromSPos p
         testTok t'@(Token typ' _)                     = if tokcmp typ typ' then Just t' else Nothing

-- toks :: [Token] -> Parser [Token]
-- toks = tokens showTok nextPos 
--    where showTok ts'   = show ts'
--          nextPos p ts' = incSourceColumn p $ length ts'

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
mktok tt = (Token tt (SPos (0,0), SPos (0,0)))

-- various token parsers
pINDENT        = tok (mktok $ TTIndent 0)                      "INDENT"
pDEDENT        = tok (mktok $ TTDedent 0)                      "DEDENT"
pNEWLINE       = tok (mktok TTNewline)                         "NEWLINE"
pKW s          = tok (mktok $ TTKeyword s)                     $ "Keyword \"" ++ s ++ "\""
pID            = tok (mktok $ TTIdentifier "")                 "Identifier"
pLTSTR         = tok (mktok $ TTLiteral $ LTString "")         "String Literal"
pLTBYTES       = tok (mktok $ TTLiteral $ LTBytes "")          "Byte Literal"
pLTINTEGER     = tok (mktok $ TTLiteral $ LTInteger "")        "Integer Literal"
pLTFLOAT       = tok (mktok $ TTLiteral $ LTFloat "")          "Float Literal"
pLTIMAGINARY   = tok (mktok $ TTLiteral $ LTImaginary "" "")   "Imaginary Literal"
pOP s          = tok (mktok $ TTOperator s)                    $ "Operator \"" ++ s ++ "\""
pDEL s         = tok (mktok $ TTDelimeter s)                   $ "Delimeter \"" ++ s ++ "\""



