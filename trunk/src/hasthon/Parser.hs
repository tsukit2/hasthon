{-# LANGUAGE TupleSections #-}
module Hasthon.Parser where

import Text.Parsec 
import qualified Text.Parsec.Prim as PS
import qualified Text.Parsec.Error as PE
import Hasthon.Scanner
import Data.Maybe
 
-- abstract syntax tree
data ParseTree = PTFoo String
               | PTFileInput [Statement]
               | PTSingleInput Statement
               | PTEvalInput Expression
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
               | STDel Expression
               | STExpr Expression
               | STAssign Expression [Expression]
               | STAugAssign Expression 
               | STAssert Expression (Maybe Expression)
               | STImportNames [([Token], Maybe Token)]
               | STImportFrom [Token] [Token] [(Token, Maybe Token)]
               | STBundle [Statement]
               | STIf (Expression,[Statement]) [(Expression,[Statement])] [Statement]
               | STWhile (Expression,[Statement]) [Statement]
               | STFor (Expression,[Statement]) [Statement]
               | STTry [Statement] [(Maybe (Expression, Maybe Token), [Statement])] [Statement] [Statement]
               | STFoo String
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
                | EXDoubleStar Expression
                | EXOr Expression Expression
                | EXXor Expression Expression
                | EXAnd Expression Expression
                | EXLeftShift Expression Expression
                | EXRightShift Expression Expression
                | EXPlus Expression Expression
                | EXMinus Expression Expression
                | EXMultiply Expression Expression
                | EXDivide Expression Expression
                | EXMod Expression Expression
                | EXIntDivide Expression Expression
                | EXPositive Expression
                | EXNegative Expression
                | EXCompliment Expression
                | EXPower Expression Expression
                | EXFuncCall Expression [Expression]
                | EXSubscipt Expression Expression
                | EXSlice (Maybe Expression) (Maybe Expression) (Maybe Expression)
                | EXMemberRef Expression Token 
                | EXYield (Maybe Expression)
                | EXGenerator Expression [ListComp]
                | EXListComp Expression [ListComp]
                | EXDictComp (Expression,Expression) [ListComp]
                | EXSetComp Expression [ListComp]
                | EXNameArg Token Expression
                | EXLambda [VarArg] Expression
                  deriving (Eq, Show)

-- list comprehension element
data ListComp = LCFor Expression Expression
              | LCIf Expression
                deriving (Eq, Show)

-- variable argument list
data VarArg = VAPlain Token (Maybe Expression)
            | VAStar (Maybe Token)
            | VADoubleStar Token
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
parse :: ParseMode -> String -> Either Hasthon.Parser.ParseError ParseTree
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
singleInputGrammar :: Parser ParseTree
singleInputGrammar = do 
   fail ""
   return $ PTFoo "singleInputGrammar"

-- file input grammar
fileInputGrammar :: Parser ParseTree
fileInputGrammar = many pStmt >>= return . PTFileInput . unbundleStmts

-- eval input grammar
evalInputGrammar :: Parser ParseTree
evalInputGrammar = do 
   fail ""
   return $ PTFoo "evalInputGrammar"

-- statement
pStmt :: Parser Statement
pStmt = pSimpleStmt <|> pCompoundStmt

-- simple statement
pSimpleStmt :: Parser Statement
pSimpleStmt = do
   rFirstStmt <- pSmallStmt
   rOtherStmts <- many (pDEL ";" >> pSmallStmt >>= return)
   optional $ pDEL ";"
   pNEWLINE
   return $ if null rOtherStmts then rFirstStmt else STBundle (rFirstStmt : rOtherStmts)

-- compound statement statement
pCompoundStmt :: Parser Statement
pCompoundStmt = pIfStmt <|> pWhileStmt <|> pForStmt <|> pTryStmt <|> pWithStmt <|>
                pFuncdef <|> pClassdef <|> pDecorated

-- if statement
pIfStmt :: Parser Statement
pIfStmt = do
   rIf <- (pKW "if" >> pTest >>= (\t -> pDEL ":" >> pSuite >>= return . (t,)))
   rElsIfs <- many (pKW "elif" >> pTest >>= (\t -> pDEL ":" >> pSuite >>= return . (t,)))
   rElse <- optionMaybe (pKW "else" >> pDEL ":" >> pSuite)
   return $ STIf rIf rElsIfs $ fromMaybe [] rElse

-- statement suit
pSuite :: Parser [Statement]
pSuite = (pSimpleStmt >>= return . unbundleStmts . (:[]))
        <|> 
         (pNEWLINE >> pINDENT >> many pStmt >>= (\s -> pDEDENT >> return s))

-- while statement
pWhileStmt :: Parser Statement
pWhileStmt = do
   rCondAndCode <- (pKW "while" >> pTest >>= (\t -> pDEL ":" >> pSuite >>= return . (t,)))
   rElse <- optionMaybe (pKW "else" >> pDEL ":" >> pSuite)
   return $ STWhile rCondAndCode $ fromMaybe [] rElse

-- for statement
pForStmt :: Parser Statement
pForStmt = do
   rCondAndcode <- (pKW "for" >> pExprlist >>= (\e -> pKW "in" >> pTestlist >>= (\l -> pDEL ":" >> 
                   pSuite >>= return . (EXIn e l,))))
   rElse <- optionMaybe (pKW "else" >> pDEL ":" >> pSuite)
   return $ STFor rCondAndcode $ fromMaybe [] rElse

-- try statement
pTryStmt :: Parser Statement
pTryStmt = do
   rTryBody <- (pKW "try" >>  pDEL ":" >> pSuite)
   ( (do rExceptClauses <- many1 (pExceptClause >>= (\e -> pDEL ":" >> pSuite >>= return . (e,)))
         rElse <- optionMaybe (pKW "else" >> pDEL ":" >> pSuite)
         rFinally <- optionMaybe (pKW "finally" >> pDEL ":" >> pSuite)
         return $ STTry rTryBody rExceptClauses (fromMaybe [] rElse) (fromMaybe [] rFinally))
     <|>
     (pKW "finally" >> pDEL ":" >> pSuite >>= return . (STTry rTryBody [] [])) )


-- except clause
pExceptClause :: Parser (Maybe (Expression, Maybe Token))
pExceptClause = do
   pKW "except"
   rExceptTest <- optionMaybe (pTest >>= (\t -> optionMaybe (pKW "as" >> pID) >>= return . (t,)))
   return rExceptTest

-- with statement
pWithStmt :: Parser Statement
pWithStmt = fail "pWithStmt"

-- function definition statement
pFuncdef :: Parser Statement
pFuncdef = fail "pFuncdef"

-- class definition statement
pClassdef :: Parser Statement
pClassdef = fail "pClassdef" 

-- decoration statement
pDecorated :: Parser Statement
pDecorated = fail "pDecorated"

-- small statement
pSmallStmt :: Parser Statement
pSmallStmt = 
   pExprStmt <|> pDelStmt <|> pPassStmt <|> pFlowStmt <|>
   pImportStmt <|> pGlobalStmt <|> pNonlocalStmt <|> pAssertStmt


-- expresion statement
pExprStmt :: Parser Statement
pExprStmt = do
   rStmt <- (pTestlistStarExpr >>= (\rExpr ->
                (pAugAssign >>= (\augop -> (pYieldExpr <|> pTestlist) >>= (\val -> return $ STAugAssign $ augop rExpr val)))
                <|>
                ((many (pDEL "=" >> (pYieldExpr <|> pTestlist))) >>= (\vals -> return $ if null vals then STExpr rExpr else STAssign rExpr vals))))
   return rStmt

-- test list star expression
pTestlistStarExpr :: Parser Expression
pTestlistStarExpr = do
   rTest <- pTestOrStar
   rMoreTests <- many $ try $ pDEL "," >> pTestOrStar
   rExtraComma <- optionMaybe $ pDEL ","
   return $ if null rMoreTests && isNothing rExtraComma
               then rTest
               else EXTuple (rTest : rMoreTests)

-- augmented assign
pAugAssign :: Parser (Expression -> Expression -> Expression)
pAugAssign = choice $ map (\(del, ex) -> pDEL del >> return ex) augops
   where augops = [
            ("+=",  EXPlus),      ("-=", EXMinus),       ("*=", EXMultiply),  ("/=", EXDivide), 
            ("%=",  EXMod),       ("&=", EXAnd),         ("|=", EXOr),        ("^=", EXXor), 
            ("<<=", EXLeftShift), (">>=", EXRightShift), ("**=", EXPower),    ("//=", EXIntDivide)
            ]
   
-- delete statement
pDelStmt :: Parser Statement
pDelStmt = do
   pKW "del"
   rExprList <- pExprlist
   return $ STDel rExprList
   
-- pass statement
pPassStmt :: Parser Statement
pPassStmt = do
   pKW "pass"
   return STPass
   
-- flow statement
pFlowStmt :: Parser Statement
pFlowStmt = pBreakStmt <|> pContinueStmt <|> pReturnStmt <|> pRaiseStmt <|> pYieldStmt

-- import statement
pImportStmt :: Parser Statement
pImportStmt = pImportName <|> pImportFrom

-- import name
pImportName :: Parser Statement
pImportName = pKW "import" >> pDottedAsNames >>= return . STImportNames

-- dotted as nameS (many)
pDottedAsNames :: Parser [([Token], Maybe Token)]
pDottedAsNames = do
   rDottedName <- pDottedAsName
   rMoreNames <- many (pDEL "," >> pDottedAsName)
   return $ rDottedName : rMoreNames

-- dotted as name (single)
pDottedAsName :: Parser ([Token], Maybe Token)
pDottedAsName = do
   rDottedName <- pDottedName
   rAsName <- optionMaybe (pKW "as" >> pID)
   return (rDottedName, rAsName)

-- dotted name
pDottedName :: Parser [Token]
pDottedName = do
   rName <- pID
   rNestedNames <- many (pDEL "." >> pID)
   return $ rName : rNestedNames


-- import from
pImportFrom :: Parser Statement
pImportFrom = do
   pKW "from"
   (rLevel, rDottedName) <- try (do rLevel <- many (pDEL "." <|> pDEL "...") 
                                    rDottedName <- pDottedName 
                                    return (rLevel, rDottedName))
                            <|>
                            (many1 (pDEL "." <|> pDEL "...") >>= (\l -> return (l, [])))
   pKW "import"
   rWhatToImport <- (pOP "*" >>= (\t -> return [(t, Nothing)]))
                    <|>
                    (pDEL "(" >> pImportAsNames >>= (\names -> pDEL ")" >> return names))
                    <|>
                    pImportAsNames
   return $ STImportFrom rLevel rDottedName rWhatToImport

-- import-as names
pImportAsNames :: Parser [(Token, Maybe Token)]
pImportAsNames = do
   rName <- pImportAsName
   rMoreNames <- many $ try $ pDEL "," >> pImportAsName
   optional $ pDEL ","
   return (rName : rMoreNames)

-- import-as name (single name)
pImportAsName :: Parser (Token, Maybe Token)
pImportAsName = do
   rName <- pID
   rAsName <- optionMaybe (pKW "as" >> pID)
   return (rName, rAsName)
   
-- global statement
pGlobalStmt :: Parser Statement
pGlobalStmt = do
   pKW "global"
   rNames <- pID `sepBy1` pDEL ","
   return $ STGlobal rNames
   
-- non-local statement
pNonlocalStmt :: Parser Statement
pNonlocalStmt = do
   pKW "nonlocal"
   rNames <- pID `sepBy1` pDEL ","
   return $ STNonlocal rNames
   
-- assert statement
pAssertStmt :: Parser Statement
pAssertStmt = do
   pKW "assert"
   rCond <- pTest
   rMsg <- optionMaybe (pDEL "," >> pTest)
   return $ STAssert rCond rMsg

-- break statment
pBreakStmt :: Parser Statement
pBreakStmt = do
   pKW "break"
   return STBreak

-- continue statment
pContinueStmt :: Parser Statement
pContinueStmt = do
   pKW "continue"
   return STContinue

-- return statement
pReturnStmt :: Parser Statement
pReturnStmt = do
   pKW "return"
   rTestlist <- optionMaybe pTestlist
   return $ STReturn rTestlist

-- raise statement
pRaiseStmt :: Parser Statement
pRaiseStmt = do
   pKW "raise" 
   rTest <- optionMaybe (do rTest' <- pTest
                            rFrom' <- optionMaybe (pKW "from" >> pTest)
                            return (rTest', rFrom'))
   return $ STRaise rTest

-- yield statement
pYieldStmt :: Parser Statement
pYieldStmt = pYieldExpr >>= (\(EXYield y) -> return $ STYield y)

-- yield expression
pYieldExpr :: Parser Expression
pYieldExpr = pKW "yield" >> optionMaybe pTestlist >>= (\y -> return $ EXYield y)


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
   rExpr <- pExprOrStar
   rMoreExprs <- many $ try $ pDEL "," >> pExprOrStar
   rExtraComma <- optionMaybe $ pDEL ","
   return $ if null rMoreExprs && isNothing rExtraComma 
               then rExpr 
               else EXTuple (rExpr : rMoreExprs)

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
pComparison = opExprParser pExpr [
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
pStarExpr = 
   pOP "*" >>  pExpr >>= (\exp -> return $ EXStar exp)
   
-- expression
pExpr :: Parser Expression
pExpr = opExprParser pXorExpr [ (pOP "|", EXOr) ]

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
   (pOP "%" , EXMod),
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
   where terminalAtom = (pID >>= return . EXId) <|> 
                        ((pLTINTEGER <|> pLTFLOAT <|> pLTIMAGINARY) >>= return . EXNumber) <|>
                        (many1 pLTSTR >>= return . EXString) <|>
                        (many1 pLTBYTES >>= return . EXByte) <|>
                        (pDEL "..." >>= return . EXEllipsis) <|>
                        (pKW "None" >>= return . EXNone) <|>
                        ((pKW "True" <|> pKW "False") >>= return . EXBoolean)
         parenAtom    = do pDEL "(" 
                           rYieldOrListComp <- optionMaybe (pYieldExpr  <|> pTestlistComp) 
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
                    ( (pCompFor >>= return . (EXDictComp rKeyValPair)) 
                      <|> 
                      (do rMoreKVPair <- many $ try $ pDEL "," >> pKeyValPair
                          optional $ pDEL ","
                          return $ EXDict (rKeyValPair : rMoreKVPair)) )
                    
                    
         pKeyValPair = pTest >>= (\k -> pDEL ":" >> pTest >>= (\v -> return (k,v)))
         pSet        = do rTest <- pTest
                          ( (pCompFor >>= return . (EXSetComp rTest)) 
                            <|>
                            (do rMoreTests <- many $ try $ pDEL "," >> pTest
                                optional $ pDEL ","
                                return $ EXSet (rTest : rMoreTests)) )


-- test list comp expression
pTestlistComp :: Parser Expression
pTestlistComp = do
   rTest <- pTestOrStar
   (
      (do rCompFor <- pCompFor
          return $ EXGenerator rTest rCompFor) 
      <|> 
      (do rMoreTests <- many $ try $ pDEL "," >> pTestOrStar
          rExtraComma <- optionMaybe $ pDEL ","
          return $ if null rMoreTests && isNothing rExtraComma
                      then rTest
                      else EXTuple (rTest : rMoreTests)) )

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
                  rTestNoCond <- pTestNoCond
                  return $ LCIf rTestNoCond
              )
   rMore <- many pCompIter
   return $ rCompIf : concat rMore

-- comprehension iteration expression
pCompIter :: Parser [ListComp]
pCompIter = pCompFor <|> pCompIf



-- trailer expression
pTrailer :: Parser (Expression -> Expression)
pTrailer = 
   choice [ (pDEL "(" >> (optionMaybe pArgList) >>= (\a -> pDEL ")" >> return (\e -> EXFuncCall e $ fromMaybe [] a))),
            (pDEL "[" >> pSubscriptList >>= (\s -> pDEL "]" >> return (\e -> EXSubscipt e s))),
            (pDEL "." >> pID >>= (\name -> return (\e -> EXMemberRef e name)))
          ]

-- argument list expression
pArgList :: Parser [Expression]
pArgList = 
   manyTill' (pArgument >>= (\a -> pDEL "," >> return a)) ( 
      try ( 
         (pArgument >>= (\a -> optional (pDEL ",") >> lookAhead (pDEL ")") >> return [a]))
         <|>
         (do pOP "*"
             rTestArg <- pTest 
             rMoreArgs <- many $ try (pDEL "," >> pArgument)
             rDoubleStarArg <- optionMaybe (pDEL "," >> pDoubleStarArg)
             let args = EXStar rTestArg : rMoreArgs in return $ maybe args (\a -> args ++ a) rDoubleStarArg
         )
         <|>
         pDoubleStarArg
      )
   )
   where -- this is a modified copy of the original manyTill to return the till part 
         manyTill' p end = scan
            where scan = ( end >>= return )
                       <|>
                         do{ x <- p; xs <- scan; return (x:xs) }
         pDoubleStarArg = pOP "**" >> pTest >>= (\a -> return [EXDoubleStar a])


-- argument expression
pArgument :: Parser Expression
pArgument = 
   try (pID >>= (\a -> pDEL "=" >> pTest >>= (\b -> return $ EXNameArg a b)))
   <|>
   (pTest >>= (\a -> optionMaybe pCompFor >>= return . (maybe a (EXGenerator a))))


-- subscript list
pSubscriptList :: Parser Expression
pSubscriptList = do
   rSubscript <- pSubscript
   rMoreSubs <- many $ try $ pDEL "," >> pSubscript
   rExtraComma <- optionMaybe $ pDEL ","
   return $ if null rMoreSubs && isNothing rExtraComma 
               then rSubscript
               else EXTuple (rSubscript : rMoreSubs)
   

-- subscript
pSubscript :: Parser Expression
pSubscript = 
   try (do pStart <- optionMaybe pTest
           pDEL ":"
           pStop <- optionMaybe pTest
           pInc <- optionMaybe pSliceOp
           return $ EXSlice pStart pStop (fromMaybe Nothing pInc))
   <|>
   pTest

-- slice op
pSliceOp :: Parser (Maybe Expression)
pSliceOp = pDEL ":" >> optionMaybe pTest


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
   pKW "lambda"
   rArgs <- optionMaybe pVarArgsList
   pDEL ":"
   rTest <- pTest
   return $ EXLambda (fromMaybe [] rArgs) rTest

-- lambda definition without condition
pLambdefNoCond :: Parser Expression
pLambdefNoCond = do
   pKW "lambda"
   rArgs <- optionMaybe pVarArgsList
   pDEL ":"
   rTestNoCond <- pTestNoCond
   return $ EXLambda (fromMaybe [] rArgs) rTestNoCond

-- variable argument list
pVarArgsList :: Parser [VarArg]
pVarArgsList = do
   (do rArg <- pPlainArg
       rMoreArgs <- many $ try (pDEL "," >> pPlainArg)
       rArrayAndDictArgs <- optionMaybe (pDEL "," >> optionMaybe pArrayAndDictArgs)
       let arrayAndDictArgs = case rArrayAndDictArgs of
                                 Nothing           -> []
                                 Just Nothing      -> []
                                 Just (Just ad)    -> ad
       return $ (rArg : rMoreArgs) ++ arrayAndDictArgs)
   <|>
   pArrayAndDictArgs
   where pArrayAndDictArgs = (do rArrayArg <- (pOP "*" >> optionMaybe pVfpDef >>= return . VAStar)
                                 rMoreArgs <- many $ try (pDEL "," >> pPlainArg)
                                 rDictArg <- optionMaybe (pDEL "," >> pDictArg)
                                 return $ (rArrayArg : rMoreArgs) ++ maybe [] (:[]) rDictArg)
                             <|>
                             (pDictArg >>= return . (:[]))
         pDictArg          = pOP "**" >> pVfpDef >>= return . VADoubleStar
         pPlainArg         = pVfpDef >>= (\a -> optionMaybe (pDEL "=" >> pTest) >>= return . (VAPlain a))


-- var argument definition
pVfpDef :: Parser Token
pVfpDef = pID

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


-- shortcut to refer to either test or star expr
pTestOrStar :: Parser Expression
pTestOrStar = pStarExpr <|> pTest

-- shortcut to refer to either expr or star expr
pExprOrStar :: Parser Expression
pExprOrStar = pStarExpr <|> pExpr

-- utility to unbundle statement
unbundleStmts :: [Statement] -> [Statement]
unbundleStmts = foldr (\x y -> case x of (STBundle s) -> s ++ y
                                         s            -> s : y) 
                      []

