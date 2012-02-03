module Hasthon.Scanner where

import Text.Parsec 
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String

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
   return (indentDedent . joinLine $ tokens)


-- join lines
joinLine :: [Token] -> [Token]
joinLine = id

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
pythonTokens = 
   lexeme True keywordToken <|> 
   lexeme False identifierToken <|> 
   lexeme False newLine <|> 
   lexeme False operatorToken <|>
   lexeme False delimeterToken <|>
   lexeme False numberToken <|> 
   stringToken <|>
   commentToken <|>
   lineJoinerToken

-- make a parser to eat up following space
lexeme :: Bool -> Parser Token -> Parser Token
lexeme sp p = do
   tok <- p
   let spfunc = if sp then many1 else many
   spfunc (char ' ') <|> (eof >> return "")
   return tok

-- keyword token
keywordToken :: Parser Token
keywordToken = do
   pos <- getPosition
   keyword <- choice keywordParsers
   return $ Token (TTKeyword keyword) pos
   where keywordParsers = map (try . string) keywords

-- identifier token
identifierToken :: Parser Token
identifierToken = do
   try (
      do
         pos <- getPosition
         firstLetter <- letter 
         rest <- many alphaNum
         let id = firstLetter : rest
         if not $ id `elem` keywords
            then return $ Token (TTIdentifier id) pos
            else fail ""
      )


-- operator token
operatorToken :: Parser Token
operatorToken = do
   pos <- getPosition
   operator <- choice operatorParsers
   return $ Token (TTOperator operator) pos
   where operatorParsers = map (try . string) operators

-- delimeter token
delimeterToken :: Parser Token
delimeterToken = do
   pos <- getPosition
   delimeter <- choice delimeterParsers
   return $ Token (TTDelimeter delimeter) pos
   where delimeterParsers = map (try . string) delimeters

-- string token
stringToken :: Parser Token
stringToken = do
   pos <- getPosition
   strs <- many1 (lexeme False (shortString <|> longString))
   let strvals = map (\(Token (TTLiteral (LTString s)) _) -> s) strs
   return $ Token (TTLiteral $ LTString $ concat strvals) pos
   where shortString = do pos <- getPosition
                          oneOf "'\"" 
                          s <- many $ noneOf "'\""
                          oneOf "'\"" 
                          return $ Token (TTLiteral $ LTString s) pos
         longString  = do pos <- getPosition
                          count 3 $ oneOf "'\"" 
                          s <- many $ noneOf "'\""
                          count 3 $ oneOf "'\"" 
                          return $ Token (TTLiteral $ LTString s) pos

   
-- number token
numberToken :: Parser Token
numberToken = 
   try ( do
      pos <- getPosition
      digits <- many1 digit
      notFollowedBy letter
      let n = read digits :: Integer
      return $ Token (TTLiteral $ LTInteger n) pos
      )

-- new line token
newLine :: Parser Token
newLine = do 
   pos <- getPosition
   try (string "\r\n") <|> string "\n" <|> string "\r" -- <|> (eof >> return "")
   return $ Token TTNewline pos

-- line joiner '\' token
lineJoinerToken :: Parser Token
lineJoinerToken = do
   pos <- getPosition
   char '\\'
   lookAhead newLine
   return $ Token TTLineJoiner pos

-- comment token
commentToken :: Parser Token
commentToken = do
   pos <- getPosition
   char '#'
   comments <- manyTill anyChar (lookAhead newLine <|> (eof >> return (Token TTNewline pos)))
   return $ Token (TTComment comments) pos

-- keywords
keywords :: [String]
keywords = [
               "False",   "None",     "True",      "and",     "as",      "assert",
               "break",   "class",    "continue",  "def",     "del",     "elif",
               "else",    "except",   "finally",   "for",     "from",    "global",
               "if",      "import",   "in",        "is",      "lambda",  "nonlocal",
               "not",     "or",       "pass",      "raise",   "return",  "try", 
               "while",   "with",     "yield"
           ]

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
               "&=",      "|=",      "^=",      ">>=",     "<<=",     "**="
             ]

