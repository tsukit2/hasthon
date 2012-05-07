{-# LANGUAGE TupleSections #-}
module Hasthon.Scanner ( 
   TokenType(..), 
   LiteralValue(..),
   Token(..),
   ScanError,
   scan,
   SPos(..),
   toSPos,
   fromSPos
   ) where


import Text.Parsec 
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Pos
import qualified Data.Set as Set
import Data.List
import Text.Parsec.Error
import Control.Monad.Error
import Control.Monad
import Text.PrettyPrint hiding (char)
import Hasthon.Common

-- token type
data TokenType = TTIndent Int
               | TTDedent Int
               | TTNewline
               | TTLineJoiner
               | TTKeyword String
               | TTIdentifier String
               | TTLiteral LiteralValue
               | TTOperator String
               | TTDelimeter String
               | TTComment String
               deriving (Eq, Show)


-- liternal value type
data LiteralValue = LTString String
                  | LTBytes String
                  | LTInteger String
                  | LTFloat String
                  | LTImaginary String String
                  deriving (Eq, Show)

-- source position
newtype SPos = SPos (Int,Int)
               deriving (Eq)

instance Show SPos where
   show (SPos p@(l,c)) = show p

-- token
data Token = Token TokenType (SPos,SPos)
             deriving (Eq)

instance PrettyPrintable Token where
   toPrettyDoc (Token tt (p,p')) = text (show tt) <+> text (show p) <> text "-" <> text (show p')

instance Show Token where
   show tok = show $ toPrettyDoc tok

-- scan error object
data ScanError = ScanError String SPos
                 deriving (Eq)

instance Show ScanError where
   show (ScanError msg _) = msg

instance Error ScanError

-- test scanner to print token in its own line
test :: String -> IO ()
test input = do
  case (scan input) of
      Right tokens   -> mapM_ (putStrLn . show) tokens
      Left err       -> print err


-- scanner start here
scan :: String -> Either ScanError [Token]
scan input = do
   case tokenResult of
      Right tokens -> indentDedent . joinLines . removeBlankLines $ tokens
      Left err     -> let message = show err
                      in Left $ ScanError message (toSPos $ errorPos err)
   where tokenResult = tokenize input


-- tokenizer
tokenize :: String -> Either ParseError [Token]
tokenize = parse (do tokens <- many pythonTokens
                     eof
                     endpos <- getPosition >>= return . toSPos
                     let endtok = Token TTNewline (endpos, endpos)
                     return (tokens ++ [endtok])) ""


-- join lines
joinLines :: [Token] -> [Token]
joinLines = joinIt 0
   where joinIt l (p@(Token (TTDelimeter delim) _) : rest) | delim `elem` [ "[", "(", "{" ] = p : joinIt (l+1) rest
                                                           | delim `elem` [ "]", ")", "}" ] = p : joinIt (l-1) rest
                                                           | otherwise                      = p : joinIt l rest
         joinIt l (nl@(Token TTNewline _) : rest)          | l > 0                          = joinIt l rest
                                                           | otherwise                      = nl : joinIt l rest
         joinIt l ((Token TTLineJoiner _) : (Token TTNewline _) : rest)                     = joinIt l rest
         joinIt l (t:rest)                                                                  = t : joinIt l rest
         joinIt _ []                                                                        = []


-- remove blank lines and comments
removeBlankLines :: [Token] -> [Token]
removeBlankLines tokens = case (removeIt tokens) of
                              ((Token TTNewline _) : rest) -> rest -- remove any starting new line
                              whatever                     -> whatever
   where removeIt (nl@(Token TTNewline _)  : (Token TTNewline _)           : rest)   = removeIt (nl:rest)
         removeIt ((Token (TTComment _) _) : rest@((Token TTNewline _)     : _))     = removeIt rest
         removeIt (nl@(Token TTNewline _)  : (Token (TTComment _) _)       : rest)   = removeIt (nl:rest)
         removeIt (token:rest)                                                       = token : removeIt rest
         removeIt []                                                                 = []

-- perform indent/dedent injection. tokens being passed are assumed to have comments and duplicate newlines removed
-- note that we use stack of minimum one element so we don't have to deal with the stack empty condition
indentDedent :: [Token] -> Either ScanError [Token]
indentDedent = xdentIt [1] True
   where xdentIt :: [Int] -> Bool -> [Token] -> Either ScanError [Token]
         xdentIt st@(p:ps) True  (tok@(Token _ (SPos pos@(l,c), _)) : rest)         
            | c > p     = xdentIt (c:st) False rest >>= (\rest -> return $ indent pos : tok : rest)
            | c < p     = dedent st pos (tok:rest)
            | otherwise = xdentIt st False rest >>= (\rest -> return $ tok : rest)
         xdentIt st@(p:ps) False (tok@(Token TTNewline (SPos pos@(l,c), _)) : rest) 
            | null rest = dedent st pos [] >>= (\rest -> return $ tok : rest)
            | otherwise = xdentIt st True rest >>= (\rest -> return $ tok : rest)
         xdentIt st        False (tok@(Token _ (SPos pos@(l,c), _)) : rest)                     
            = xdentIt st False rest >>= (\rest -> return $ tok : rest)
         xdentIt _         _     []                                                             
            = return []
         indent (l,c) = Token (TTIndent c) (SPos(l,c), SPos(l,c))
         --dedent :: [Int] -> (Int,Int) -> [Token] -> Either ScanError [Token]
         dedent st@(p:ps) pos@(l,c) rest = 
               if null rest || c `elem` st
                  then doDedent st pos rest
                  else throwError (ScanError ("unindent does not match any outer indentation level at " ++ (show pos)) 
                                             $ SPos pos)
            where doDedent st@(p:ps) pos@(l,c) rest 
                     | c < p     = dedent ps pos rest >>= (\rest -> return $ (Token (TTDedent p) (SPos pos, SPos pos)) : rest)
                     | otherwise = if null rest
                           then return $ map (\p' -> (Token (TTDedent p') (SPos pos, SPos pos))) $ init st
                           else xdentIt st False rest

-- root grammar of python tokens
pythonTokens :: Parser Token
pythonTokens = do
   pos <- getPosition
   (tokenType, pos') <- lexeme False keywordToken <|> 
                        lexeme False identifierToken <|> 
                        lexeme False newLine <|> 
                        lexeme False operatorAndDelimeterToken <|>
                        lexeme False numberToken <|> 
                        lexeme False stringToken <|>
                        getTokAndPos commentToken  <|>
                        getTokAndPos lineJoinerToken
   return (Token tokenType (toSPos pos, toSPos pos'))

-- make a parser to eat up following space and return tokent type and original
-- ending position prior the eaten space
lexeme :: Bool -> Parser TokenType -> Parser (TokenType, SourcePos)
lexeme sp p = do
   tokenType <- p
   pos <- getPosition
   let spfunc = if sp then many1 else many
   spfunc (char ' ') <|> (eof >> return "")
   return (tokenType, pos)


-- utility to get token type and pos
getTokAndPos :: Parser TokenType -> Parser (TokenType, SourcePos)
getTokAndPos p = do
   tokenType <- p
   pos <- getPosition
   return (tokenType, pos)

-- identifier token
identifierToken :: Parser TokenType
identifierToken = do
   try (
      do
         firstLetter <- letter 
         rest <- many alphaNum
         let id = firstLetter : rest
         if not $ id `Set.member` keywordsSet
            then return $ TTIdentifier id
            else fail "not an identifier"
      )

-- keyword token
keywordToken :: Parser TokenType
keywordToken = setBasedToken (do kw <- many letter
                                 notFollowedBy alphaNum
                                 return kw) keywordsSet TTKeyword "not a keyword"

-- operator and delimiter tokens
operatorAndDelimeterToken :: Parser TokenType
operatorAndDelimeterToken = strBasedToken (opers ++ delims) "neither operator nor delimeter"
   where opers  = map (,TTOperator) operators
         delims = map (,TTDelimeter) delimeters

-- string token
stringToken :: Parser TokenType
stringToken = do
   str <- (try longString <|> shortString)
   return $ TTLiteral $ LTString str
   where shortString = do oneOf "'\"" 
                          s <- many $ noneOf "'\""
                          oneOf "'\"" 
                          return s
         longString  = do count 3 $ oneOf "'\"" 
                          s <- many $ noneOf "'\""
                          count 3 $ oneOf "'\"" 
                          return s

   
-- number token
numberToken :: Parser TokenType
numberToken = 
   try ( do
      digits <- many1 digit
      notFollowedBy letter
      return $ TTLiteral $ LTInteger digits
      )

-- new line token
newLine :: Parser TokenType
newLine = do 
   try (string "\r\n") <|> string "\n" <|> string "\r" -- <|> (eof >> return "")
   return TTNewline

-- line joiner '\' token
lineJoinerToken :: Parser TokenType
lineJoinerToken = do
   char '\\'
   lookAhead newLine
   return $ TTLineJoiner

-- comment token
commentToken :: Parser TokenType
commentToken = do
   char '#'
   comments <- manyTill anyChar (lookAhead newLine <|> (eof >> return TTNewline))
   return $ TTComment comments

-- keywords
keywords = [
               "False",   "None",     "True",      "and",     "as",      "assert",
               "break",   "class",    "continue",  "def",     "del",     "elif",
               "else",    "except",   "finally",   "for",     "from",    "global",
               "if",      "import",   "in",        "is",      "lambda",  "nonlocal",
               "not",     "or",       "pass",      "raise",   "return",  "try", 
               "while",   "with",     "yield"
           ]

keywordsSet = Set.fromList keywords

-- operators
operators = [
               "+",      "-",       "*",       "**",      "/",       "//",      "%",
               "<<",     ">>",      "&",       "|",       "^",       "~",
               "<",      ">",       "<=",      ">=",      "==",      "!="
            ]

-- delimeters
delimeters = [
               "(",       ")",       "[",       "]",       "{",       "}",
               ",",       ":",       ".",       ";",       "@",       "=",
               "+=",      "-=",      "*=",      "/=",      "//=",     "%=",
               "&=",      "|=",      "^=",      ">>=",     "<<=",     "**=",
               "...",     "->"
             ]

-- utility to convert between SourcePosition and our SPos
toSPos :: SourcePos -> SPos
toSPos p = SPos (sourceLine p, sourceColumn p)

fromSPos :: SPos -> SourcePos
fromSPos (SPos (l,c)) = newPos "" l c

-- set-based token type (utility to help create token based on set
setBasedToken :: Parser String -> Set.Set String -> (String -> TokenType) -> String -> Parser TokenType
setBasedToken p s f em = do
   try (
      do
         tokstr <- p
         if tokstr `Set.member` s 
            then return $ f tokstr
            else fail em
       )

-- string-based token type (utility function to make choice parser for longest string first)
-- string-based token type that can mix different token type together
strBasedToken :: [(String,(String -> TokenType))] -> String -> Parser TokenType
strBasedToken strFuncs errmsg = 
   (choice $ map (\(s,f) -> try (string s) >>= return . f) sortedStrFuncs) <?> errmsg
   where comp (l,_) (l',_) =  compare l' l
         sortedStrFuncs = snd $ unzip $ sortBy comp strFuncsWithLen
         strFuncsWithLen = zip (map (length . fst) strFuncs) strFuncs

