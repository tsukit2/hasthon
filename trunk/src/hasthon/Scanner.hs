module Hasthon.Scanner where

import Text.Parsec 
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import qualified Data.Set as Set

-- token type
data TokenType = TTIndent
               | TTDedent
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
                  | LTInteger Integer
                  | LTFloat Double
                  | LTImaginary Double Int
                  deriving (Eq, Show)

-- token
data Token = Token TokenType SourcePos
             deriving (Eq, Show)

-- test scanner to print token in its own line
test :: String -> IO ()
test input = do
  case (scanner input) of
      Right tokens   -> mapM_ (putStrLn . show) tokens
      Left err       -> print err


-- scanner start here
scanner :: String -> Either ParseError [Token]
scanner input = do
   tokens <- tokenize input
   return (indentDedent . joinLines . removeBlankLines $ tokens)


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

-- perform indent/dedent injection
indentDedent :: [Token] -> [Token]
indentDedent = id

-- tokenizer
tokenize :: String -> Either ParseError [Token]
tokenize = parse (do tokens <- many pythonTokens
                     eof
                     return tokens) ""

-- root grammar of python tokens
pythonTokens :: Parser Token
pythonTokens = do
   pos <- getPosition
   tokenType <- lexeme True keywordToken <|> 
                lexeme False identifierToken <|> 
                lexeme False newLine <|> 
                lexeme False operatorToken <|>
                lexeme False delimeterToken <|>
                lexeme False numberToken <|> 
                lexeme False stringToken <|>
                commentToken <|>
                lineJoinerToken
   return (Token tokenType pos)

-- make a parser to eat up following space
lexeme :: Bool -> Parser TokenType -> Parser TokenType
lexeme sp p = do
   tokenType <- p
   let spfunc = if sp then many1 else many
   spfunc (char ' ') <|> (eof >> return "")
   return tokenType

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
keywordToken = setBasedToken (many letter) keywordsSet TTKeyword "not a keyword"

-- operator token
operatorToken :: Parser TokenType
operatorToken = setBasedToken (many (oneOf operatorChars)) operatorsSet TTOperator "not an operator"

-- delimeter token
delimeterToken :: Parser TokenType
delimeterToken = setBasedToken (many (oneOf delimeterChars)) delimetersSet TTDelimeter "not a delimeter"

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

-- string token
stringToken :: Parser TokenType
stringToken = do
   str <- (try longString <|> shortString)
   return $ TTLiteral $ LTString str
   where shortString = do pos <- getPosition
                          oneOf "'\"" 
                          s <- many $ noneOf "'\""
                          oneOf "'\"" 
                          return s
         longString  = do pos <- getPosition
                          count 3 $ oneOf "'\"" 
                          s <- many $ noneOf "'\""
                          count 3 $ oneOf "'\"" 
                          return s

   
-- number token
numberToken :: Parser TokenType
numberToken = 
   try ( do
      digits <- many1 digit
      notFollowedBy letter
      let n = read digits :: Integer
      return $ TTLiteral $ LTInteger n
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

operatorsSet = Set.fromList operators

operatorChars = Set.toList $ Set.fromList $ concat operators

-- delimeters
delimeters = [
               "(",       ")",       "[",       "]",       "{",       "}",
               ",",       ":",       ".",       ";",       "@",       "=",
               "+=",      "-=",      "*=",      "/=",      "//=",     "%=",
               "&=",      "|=",      "^=",      ">>=",     "<<=",     "**="
             ]

delimetersSet = Set.fromList delimeters

delimeterChars = Set.toList $ Set.fromList $ concat delimeters

