{-# LANGUAGE TupleSections #-}
module Hasthon.SemanticAnalyzer ( 

   ) where


--         convert :: Maybe Char -> String -> Parser String
--         convert p s      = if isJust p then return s else doConvert s 0
--         doConvert :: String -> Int -> Parser String
--         doConvert s p    = case s of
--                               ('\\' : l : r)   -> 
--                                    case (l:r) of
--                                       ('\n' : r)  -> doConvert r (p+2)
--                                       ('\'' : r)  -> doConvert r (p+2) >>= return . ('\'':)
--                                       ('"'  : r)  -> doConvert r (p+2) >>= return . ('"':)
--                                       ('a'  : r)  -> doConvert r (p+2) >>= return . ('\a':)
--                                       ('b'  : r)  -> doConvert r (p+2) >>= return . ('\b':)
--                                       ('f'  : r)  -> doConvert r (p+2) >>= return . ('\f':)
--                                       ('n'  : r)  -> doConvert r (p+2) >>= return . ('\n':)
--                                       ('t'  : r)  -> doConvert r (p+2) >>= return . ('\t':)
--                                       ('v'  : r)  -> doConvert r (p+2) >>= return . ('\v':)
--                                       ('x'  : r)  -> doConvertHex r (p+2)
--                                       r           -> doConvertOct r p
--                               (l    : r)       -> doConvert r (p+1) >>= return . (l:)
--                               []               -> return []
--         doConvertHex (h1:h2:r) p | isHexDigit h1 && isHexDigit h2 = doConvert r (p+2) >>= return . ((chr $ toHex (h1:h2:[])):)
--                                  | otherwise                      = fail $ "escaped ecode error at position: " ++ show p ++ "-" ++ show (p+2)
--         doConvertOct s p = doConvert s (p+1)
--         toHex s          = let l = (length s) - 1 in foldl (\a (c,p) -> (digitToInt c `shiftL` (4*p)) + a) 0 (zip s [l,l-1,0])

-- \newline 	Backslash and newline ignored 	 
-- \\ 	Backslash (\) 	 
-- \' 	Single quote (') 	 
-- \" 	Double quote (") 	 
-- \a 	ASCII Bell (BEL) 	 
-- \b 	ASCII Backspace (BS) 	 
-- \f 	ASCII Formfeed (FF) 	 
-- \n 	ASCII Linefeed (LF) 	 
-- \r 	ASCII Carriage Return (CR) 	 
-- \t 	ASCII Horizontal Tab (TAB) 	 
-- \v 	ASCII Vertical Tab (VT) 	 
-- \ooo 	Character with octal value ooo 	(1,3)
-- \xhh 	Character with hex value hh 	(2,3)

