module Hasthon.Parser where

import Text.Parsec 
import qualified Text.Parsec.Prim as PS
import qualified Text.Parsec.Error as PE
import Hasthon.Scanner
import Data.Maybe
 
-- abstract syntax tree
data ABSTree = ABSTree String
             | ABSStmt Statement
             | Stmts [ABSTree]
               deriving (Eq, Show)


-- statement
data Statement = STPass 
               | STBreak
               | STContinue
               | STReturn (Maybe Expression)
               | STRaise (Maybe (Expression, Maybe Expression))
               | STYield (Maybe Expression)
               | STGlobal [Token]
               | STNonlocal [Token]
                 deriving (Eq, Show)

-- expresion
data Expression = EXString [Token]
                | EXByte [Token]
                | EXNumber Token
                | EXId Token 
                | EXEllipsis Token
                | EXBoolean Token
                | EXNone Token
                | EXTuple [Expression]
                | EXList [Expression]
                | EXDict [(Expression,Expression)]
                | EXSet [Expression]
                | EXTest Expression Expression Expression
                | EXOrTest Expression Expression
                | EXAndTest Expression Expression
                | EXNotTest Expression
                | EXLessThan Expression Expression
                | EXGreaterThan Expression Expression 
                | EXEqual Expression Expression 
                | EXGreaterThanOrEqual Expression Expression
                | EXLessThanOrEqual Expression Expression
                | EXNotEqual Expression Expression
                | EXIn Expression Expression
                | EXNotIn Expression Expression
                | EXIs Expression Expression
                | EXIsNot Expression Expression
                | EXStar Expression
                | EXOr Expression Expression
                | EXXor Expression Expression
                | EXAnd Expression Expression
                | EXLeftShift Expression Expression
                | EXRightShift Expression Expression
                | EXPlus Expression Expression
                | EXMinus Expression Expression
                | EXMultiply Expression Expression
                | EXDivide Expression Expression
                | EXReminder Expression Expression
                | EXIntDivide Expression Expression
                | EXPositive Expression
                | EXNegative Expression
                | EXCompliment Expression
                | EXPower Expression Expression
                | EXFuncCall Expression [Expression]
                | EXSubscipt Expression [Expression]
                | EXMemberRef Expression Token 
                | EXYield (Maybe Expression)
                | EXGenerator Expression [ListComp]
                | EXListComp Expression [ListComp]
                | EXDictComp (Expression,Expression) [ListComp]
                | EXSetComp Expression [ListComp]
                | EXAtom Token
                  deriving (Eq, Show)

-- list comprehension element
data ListComp = LCFor Expression Expression
              | LCIf Expression
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
   rStmts <- many pStmt
   return $ if null rStmts then ABSTree "fileInputGrammar" else head rStmts

-- eval input grammar
evalInputGrammar :: Parser ABSTree
evalInputGrammar = do 
   fail ""
   return $ ABSTree "evalInputGrammar"

-- statement
pStmt :: Parser ABSTree
pStmt = pSimpleStmt <|> pCompoundStmt

-- simple statement
pSimpleStmt :: Parser ABSTree
pSimpleStmt = do
   rFirstStmt <- pSmallStmt
   rOtherStmts <- many (pDEL ";" >> pSmallStmt >>= return)
   optional $ pDEL ";"
   pNEWLINE
   return $ Stmts (rFirstStmt : rOtherStmts)

-- compound statement statement
pCompoundStmt :: Parser ABSTree
pCompoundStmt = do
   fail ""
   return $ ABSTree "compoundStmt"

-- small statement
pSmallStmt :: Parser ABSTree
pSmallStmt = 
   pExprStmt <|> pDelStmt <|> pPassStmt <|> pFlowStmt <|>
      pImportStmt <|> pGlobalStmt <|> pNonlocalStmt <|> pAssertStmt


-- expresion statement
pExprStmt :: Parser ABSTree
pExprStmt = do
   fail ""
   return $ ABSTree "exprStmt"
   
-- delete statement
pDelStmt :: Parser ABSTree
pDelStmt = do
   fail ""
   return $ ABSTree "delStmt"
   
-- pass statement
pPassStmt :: Parser ABSTree
pPassStmt = do
   pKW "pass"
   return $ ABSStmt STPass
   
-- flow statement
pFlowStmt :: Parser ABSTree
pFlowStmt = pBreakStmt <|> pContinueStmt <|> pReturnStmt <|> pRaiseStmt <|> pYieldStmt

-- import statement
pImportStmt :: Parser ABSTree
pImportStmt = do
   fail ""
   return $ ABSTree "importStmt"
   
-- global statement
pGlobalStmt :: Parser ABSTree
pGlobalStmt = do
   pKW "global"
   rNames <- pID `sepBy1` pDEL ","
   return $ ABSStmt $ STGlobal rNames
   
-- non-local statement
pNonlocalStmt :: Parser ABSTree
pNonlocalStmt = do
   pKW "nonlocal"
   rNames <- pID `sepBy1` pDEL ","
   return $ ABSStmt $ STNonlocal rNames
   
-- assert statement
pAssertStmt :: Parser ABSTree
pAssertStmt = do
   fail ""
   return $ ABSTree "assertStmt"

-- break statment
pBreakStmt :: Parser ABSTree
pBreakStmt = do
   pKW "break"
   return $ ABSStmt STBreak

-- continue statment
pContinueStmt :: Parser ABSTree
pContinueStmt = do
   pKW "continue"
   return $ ABSStmt STContinue

-- return statement
pReturnStmt :: Parser ABSTree
pReturnStmt = do
   pKW "return"
   rTestlist <- optionMaybe pTestlist
   return $ ABSStmt $ STReturn rTestlist

-- raise statement
pRaiseStmt :: Parser ABSTree
pRaiseStmt = do
   pKW "raise" 
   rTest <- optionMaybe (do rTest' <- pTest
                            rFrom' <- optionMaybe (pKW "from" >> pTest)
                            return (rTest', rFrom'))
   return $ ABSStmt $ STRaise rTest

-- yield statement
pYieldStmt :: Parser ABSTree
pYieldStmt = pYieldExpr >>= (return . ABSStmt . STYield)

-- yield expression
pYieldExpr :: Parser (Maybe Expression)
pYieldExpr = pKW "yield" >> optionMaybe pTestlist


-- test list epression
pTestlist :: Parser Expression
pTestlist = do
   rTest <- pTest
   rMoreTests <- many $ try $ pDEL "," >> pTest
   rExtraComma <- optionMaybe $ pDEL ","
   return $ if null rMoreTests && isNothing rExtraComma
               then rTest
               else EXTuple (rTest : rMoreTests)

-- expression list
pExprlist :: Parser Expression
pExprlist = do
   rStarExpr <- pStarExpr 
   rMoreStarExprs <- many $ try $ pDEL "," >> pStarExpr
   rExtraComma <- optionMaybe $ pDEL ","
   return $ if null rMoreStarExprs && isNothing rExtraComma 
               then rStarExpr 
               else EXTuple (rStarExpr : rMoreStarExprs)

-- test expression
pTest :: Parser Expression
pTest = 
   (do rOrTest <- pOrTest
       rIfCond <- optionMaybe (do pKW "if" 
                                  rIfCond <- pOrTest
                                  pKW "else"
                                  rElse <- pTest
                                  return (rIfCond, rElse))
       return $ case rIfCond of
                   Nothing                   -> rOrTest
                   Just (rIfCond,rElse)      -> EXTest rOrTest rIfCond rElse
   ) <|> pLambdef 

-- test no condition
pTestNoCond :: Parser Expression
pTestNoCond = pOrTest <|> pLambdefNoCond

-- or-test expression
pOrTest :: Parser Expression
pOrTest = opExprParser pAndTest [ (pKW "or", EXOrTest) ]

-- and-test expression
pAndTest :: Parser Expression
pAndTest = opExprParser pNotTest [ (pKW "and", EXAndTest) ]

-- not-test expression
pNotTest :: Parser Expression
pNotTest = (pKW "not" >> pNotTest >>= (\ex -> return $ EXNotTest ex)) <|> pComparison

-- comparision expression
pComparison :: Parser Expression
pComparison = opExprParser pStarExpr [
   (pOP "<"               , EXLessThan),
   (pOP ">"               , EXGreaterThan),
   (pOP "=="              , EXEqual),
   (pOP ">="              , EXGreaterThanOrEqual),
   (pOP "<="              , EXLessThanOrEqual),
   (pOP "!="              , EXNotEqual),
   (pKW "in"              , EXIn),
   (pKW "not" >> pKW "in" , EXNotIn),
   (pKW "is"              , EXIs),
   (pKW "is" >> pKW "not" , EXIsNot)
   ]

-- star expression
pStarExpr :: Parser Expression
pStarExpr = do
   rStar <- optionMaybe $ pOP "*"
   case rStar of
      Just _   -> pExpression >>= (\exp -> return $ EXStar exp)
      Nothing  -> pExpression 
   
-- expression
pExpression :: Parser Expression
pExpression = opExprParser pXorExpr [ (pOP "|", EXOr) ]

-- xor expression
pXorExpr :: Parser Expression
pXorExpr = opExprParser pAndExpr [ (pOP "^", EXXor) ]

-- and expression
pAndExpr :: Parser Expression
pAndExpr = opExprParser pShiftExpr [ (pOP "&", EXAnd) ]


-- shift expression
pShiftExpr :: Parser Expression
pShiftExpr = opExprParser pArithExpr [
   (pOP "<<", EXLeftShift),
   (pOP ">>", EXRightShift)
   ]

-- arithmatic expression
pArithExpr :: Parser Expression
pArithExpr = opExprParser pTerm [
   (pOP "+", EXPlus),
   (pOP "-", EXMinus)
   ]

-- term expression
pTerm :: Parser Expression
pTerm = opExprParser pFactor [
   (pOP "*" , EXMultiply),
   (pOP "/" , EXDivide),
   (pOP "%" , EXReminder),
   (pOP "//", EXIntDivide)
   ]

-- factor expression
pFactor :: Parser Expression
pFactor = 
   (do rPrefix <- (pOP "+" >> return EXPositive) <|> 
                  (pOP "-" >> return EXNegative) <|>
                  (pOP "~" >> return EXCompliment)
       rFactor <- pFactor
       return $ rPrefix rFactor
   ) <|> pPower

-- power expression
pPower :: Parser Expression
pPower = do
   rAtom <- pAtom
   rTrailers <- many pTrailer
   rAtomModifiers <- option rTrailers (pOP "**" >> pFactor >>= (\e -> return $ rTrailers ++ [(\x -> EXPower x e)]))
   return $ if null rAtomModifiers 
               then rAtom
               else foldl (\x f -> f x) rAtom rAtomModifiers

-- atom expression
pAtom :: Parser Expression
pAtom = 
   parenAtom <|> squareAtom <|> braceAtom <|> terminalAtom
   where terminalAtom = (pID >>= (return . EXId)) <|> 
                        ((pLTINTEGER <|> pLTFLOAT <|> pLTIMAGINARY) >>= (return . EXNumber)) <|>
                        (many1 pLTSTR >>= (return . EXString)) <|>
                        (many1 pLTBYTES >>= (return . EXByte)) <|>
                        (pDEL "..." >>= (return . EXEllipsis)) <|>
                        (pKW "None" >>= (return . EXNone)) <|>
                        ((pKW "True" <|> pKW "False") >>= (return . EXBoolean))
         parenAtom    = do pDEL "(" 
                           rYieldOrListComp <- optionMaybe ((pYieldExpr >>= (return . EXYield)) <|> pTestlistComp) 
                           pDEL ")" 
                           return $ fromMaybe (EXTuple []) rYieldOrListComp
         squareAtom   = do pDEL "["
                           rListComp <- optionMaybe pTestlistComp
                           pDEL "]"
                           return $ case rListComp of
                                       Nothing                 -> EXList []
                                       Just (EXTuple exprs)    -> EXList exprs
                                       Just (EXGenerator a b)  -> EXListComp a b
                                       Just sthelse            -> EXList [sthelse]
         braceAtom    = do pDEL "{"
                           rSetOrDict <- optionMaybe pDictOrSetMaker
                           pDEL "}"
                           return $ fromMaybe (EXDict []) rSetOrDict

-- dictionary or set maker expression
pDictOrSetMaker :: Parser Expression
pDictOrSetMaker = try pDict <|> pSet
   where pDict = do rKeyValPair <- pKeyValPair
                    ( (pCompFor >>= (return . (EXDictComp rKeyValPair))) 
                      <|> 
                      (do rMoreKVPair <- many $ try $ pDEL "," >> pKeyValPair
                          optional $ pDEL ","
                          return $ EXDict (rKeyValPair : rMoreKVPair)) )
                    
                    
         pKeyValPair = pTest >>= (\k -> pDEL ":" >> pTest >>= (\v -> return (k,v)))
         pSet        = do rTest <- pTest
                          ( (pCompFor >>= (return . (EXSetComp rTest))) 
                            <|>
                            (do rMoreTests <- many $ try $ pDEL "," >> pTest
                                optional $ pDEL ","
                                return $ EXSet (rTest : rMoreTests)) )


-- test list comp expression
pTestlistComp :: Parser Expression
pTestlistComp = 
   try (do rTest <- pTest
           rCompFor <- pCompFor
           return $ EXGenerator rTest rCompFor
   ) <|> pTestlist

-- comprehension for expression
pCompFor :: Parser [ListComp]
pCompFor = do
   rCompFor <- (do pKW "for"
                   rExplist <- pExprlist
                   pKW "in"
                   rOrTest <- pOrTest
                   return $ LCFor rExplist rOrTest
               )
   rMore <- many pCompIter
   return $ rCompFor : concat rMore

-- comprehension if expression
pCompIf :: Parser [ListComp]
pCompIf = do
   rCompIf <- (do pKW "if"
                  rTestCond <- pTestNoCond
                  return $ LCIf rTestCond
              )
   rMore <- many pCompIter
   return $ rCompIf : concat rMore

-- comprehension iteration expression
pCompIter :: Parser [ListComp]
pCompIter = pCompFor <|> pCompIf



-- trailer expression
pTrailer :: Parser (Expression -> Expression)
pTrailer = 
   choice [ (pDEL "(" >> pDEL ")" >> return (\e -> EXFuncCall e [])),
            (pDEL "[" >> pDEL "]" >> return (\e -> EXSubscipt e [])),
            (pDEL "." >> pID >>= (\name -> return (\e -> EXMemberRef e name)))
          ]


-- utility function to parse repeated expression of different operators but same precedence
opExprParser :: (Parser Expression) -> [((Parser Token), (Expression -> Expression -> Expression))] -> Parser Expression
opExprParser p ops = do
   rP <- p
   rest <- many $ choice optionalParsers
   return $ if null rest 
               then rP
               else foldl (\x f -> f x) rP rest
   where optionalParsers :: [Parser (Expression -> Expression)]
         optionalParsers = map toMany1 ops
         toMany1 :: (Parser Token, (Expression -> Expression -> Expression)) -> Parser (Expression -> Expression)
         toMany1 (op,f)  = (op >> p) >>= (\x -> return $ (flip f) x)

-- lambda definition
pLambdef :: Parser Expression
pLambdef = do
   fail "not supported yet"

-- lambda definition with no condition
pLambdefNoCond :: Parser Expression
pLambdefNoCond = do
   fail "not supported yet"


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



