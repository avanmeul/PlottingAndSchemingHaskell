{-
Andr� van Meulebrouck
scheme.hs
-}

import Text.Regex.Posix

{--
*Main> tokenize "(car '(\"sdfdsfds\" 3 arsd\"sd()fdsf\"))"
[LeftParen,Atom "car",SingleQuote,LeftParen,Str "sdfdsfds",Whitespace " ",Atom "3",Atom "arsd",Str "sd()fdsf",RightParen,RightParen]

*Main> tokenize "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsfdsf()dfssdf)\" abc(sd sd d)))"
[LeftParen,Atom "define",SingleQuote,Atom "foo",Atom "sdfsdf.sdfsdfds",Atom "sdfds",Str "dsfdsf",Whitespace " ",Atom "3",Atom "-3",Str "fdsfdsf()dfssdf)",Whitespace " ",Atom "abc",Atom "sd",Atom "sd",Atom "d",RightParen,RightParen]

*Main> tokenize "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsf'dsf()dfssdf)\" abc(sd sd d)))"
[LeftParen,Atom "define",Atom "foo",SingleQuote,LeftParen,Atom "sdfsdf.sdfsdfds",Atom "sdfds",Str "dsfdsf",Whitespace " ",Atom "3",Atom "-3",Str "fdsf'dsf()dfssdf)",Whitespace " ",Atom "abc",Atom "sd",Atom "sd",Atom "d",RightParen,RightParen]
--}

data TokenType =
    LeftTok |
    RightTok |
    SingleTok |
    DoubleTok |
    WhiteTok |
    AlphaTok |
    IntTok |
    DotTok |
    DashTok 
    deriving (Eq, Show)

data Token =
    LeftParen |
    RightParen |
    Atom String | --deprecated
    Dot |
    Symbol String |
    Integer String |
    Float String |
    Whitespace String |
    Str String|
    SingleQuote 
    deriving (Show)

data InProcess = 
	AtomProc |
	StringProc |
	WhiteProc | 
	CommentProc |
	NothingProc
	deriving (Show)

{-- 
in process [whitespace, string, comment, atom]
tok (token substring in process)
in process enumerations can be stacked for nested cases
--}

isLeftParen :: Char -> Bool
isLeftParen x = x == '('

isRightParen :: Char -> Bool
isRightParen x = x == ')'

isSingleQuote :: Char -> Bool
isSingleQuote x = x == '\''

isAlphabetic :: Char -> Bool
--isAlphabetic x = elem x (['a'..'z'] ++ ['A'..'Z'])
isAlphabetic x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')

isInteger :: Char -> Bool
--isInteger x = elem x ['0'..'9']
isInteger x = x >= '0' && x <= '9'

isDash :: Char -> Bool
isDash x = x == '-'

isDot :: Char -> Bool
isDot x = x == '.'

isDoubleQuote :: Char -> Bool
isDoubleQuote x = x == '"'

getTokType :: Char -> TokenType
getTokType x = 
	case () of 
		_ | isLeftParen x -> LeftTok
		_ | isRightParen x -> RightTok
		_ | isSingleQuote x -> SingleTok
		_ | isDoubleQuote x -> DoubleTok
		_ | isWhitespace x -> WhiteTok
		_ | isAlphabetic x -> AlphaTok
		_ | isInteger x -> IntTok
		_ | isDot x -> DotTok
		_ | isDash x -> DashTok
		_ | otherwise -> error "unknown token type"

tokenize :: String -> [Token]
tokenize s = tokenize' s ("", NothingProc) []

tokenize' :: String -> (String, InProcess) -> [Token] -> [Token]
tokenize' [] tup tokens = reverse tokens --need to check for in proc
tokenize' (h:t) tup tokens =
	case (getTokType h) of 
		DoubleTok ->
			case snd tup of
				AtomProc -> 
					tokenize' t ("", StringProc) ((Atom (reverse (fst tup))) : tokens)
				StringProc ->
					tokenize' t ("", NothingProc) ((Str (reverse (fst tup))) : tokens)
				otherwise ->
					tokenize' t ("", StringProc) tokens
		LeftTok ->
			case snd tup of 
				StringProc -> 
					tokenize' t (h:fst tup, StringProc) tokens
				AtomProc -> 
					tokenize' t ("", NothingProc) ((Atom (reverse (fst tup))) : tokens)
				otherwise -> 
					tokenize' t ("", NothingProc) (LeftParen : tokens)
		RightTok -> 
			case snd tup of 
				StringProc -> 
					tokenize' t (h:fst tup, StringProc) tokens
				AtomProc -> 
					tokenize' t ("", NothingProc) ((Atom (reverse (fst tup))) : tokens)
				otherwise -> 
					tokenize' t ("", NothingProc) (RightParen : tokens)
		SingleTok -> 
			case snd tup of 
				StringProc -> 
					tokenize' t (h:fst tup, StringProc) tokens
				AtomProc -> 
					tokenize' t ("", NothingProc) (SingleQuote : (Atom (reverse (fst tup))) : tokens)
				otherwise -> 
					tokenize' t ("", NothingProc) (SingleQuote : tokens)
		WhiteTok -> -- to do:  make this stuff into a function that takes a tuple and a list of tokens
			case snd tup of 
				AtomProc -> 
					tokenize' t ("", NothingProc) ((Atom (reverse (fst tup))) : tokens)
				otherwise -> 
					tokenize' t ("", NothingProc) (Whitespace [h] : tokens)
		otherwise ->
			case snd tup of 
				NothingProc ->
					tokenize' t (h:fst tup, AtomProc) tokens		
				otherwise ->
					tokenize' t (h:fst tup, snd tup) tokens

-- new tokenizer

isCharSymbol :: Char -> Bool
isCharSymbol c = 
	(c >= 'a' && c <= 'z') || 
	(c >= 'A' && c <= 'Z') ||
	(c >= '0' && c <= '9') ||
	(c == '-') ||
	(c == '_') ||
	(c == '.')

isWhitespace :: Char -> Bool
isWhitespace x = elem x " \n\t"

parseSingleTok :: String -> (Maybe Token, String)
parseSingleTok s =
	case s of
		(h:t) -> 
			if h == '\'' 
			then (Just SingleQuote, drop 1 s)
			else (Nothing, s)
		[] -> (Nothing, s)

parseLeftParen :: String -> (Maybe Token, String)
parseLeftParen [] = (Nothing, [])
parseLeftParen s =
	if head s == '('
	then (Just LeftParen, tail s)
	else (Nothing, s)

parseRightParen :: String -> (Maybe Token, String)
parseRightParen [] = (Nothing, [])
parseRightParen s =
	if head s == ')'
	then (Just RightParen, tail s)
	else (Nothing, s)

parseSingleQuote :: String -> (Maybe Token, String)
parseSingleQuote [] = (Nothing, [])
parseSingleQuote s =
	if head s == '\''
	then (Just SingleQuote, tail s)
	else (Nothing, s)

parseInteger :: String -> (Maybe Token, String)
parseInteger s = 
	let m = s =~ "^[+|-]?[[:digit:]]+" :: String in
	if m == "" 
	then (Nothing, s)
	else (Just (Integer m), drop (length m) s)

parseFloat :: String -> (Maybe Token, String)
parseFloat s = 
	let m = s =~ "^[+|-]?[[:digit:]]+[.][[:digit:]]*" :: String in
	if m == "" 
	then (Nothing, s)
	else (Just (Float m), drop (length m) s)

parseDot :: String -> (Maybe Token, String)
parseDot [] = (Nothing, [])
parseDot s =
	if head s == '.'
	then (Just Dot, tail s)
	else (Nothing, s)
	
parseSymbol :: String -> (Maybe Token, String)
parseSymbol [] = (Nothing, [])
parseSymbol s =
	let sym = takeWhile isCharSymbol s in 
	if sym == "" then (Nothing, s)
	else (Just (Symbol sym), drop (length sym) s)

parseWhitespace :: String -> (Maybe Token, String)
parseWhitespace s = 
	let sym = takeWhile isWhitespace s in
	if sym == "" then (Nothing, s)
	else (Just (Whitespace sym), drop (length sym) s)

parseFunctions :: [String -> (Maybe Token, String)]
parseFunctions = 
	[parseLeftParen
	,parseRightParen
	,parseSingleQuote
	,parseWhitespace
	,parseFloat
	,parseInteger
	,parseDot
	,parseSymbol]

parse :: String -> [Token]
parse [] = []
parse s = parse' parseFunctions s []

parse' :: [String -> (Maybe Token, String)] -> String -> [Token] -> [Token]
parse' [] s tokens = error $ "unparsable expression:  " ++ s
parse' parsers [] tokens = reverse tokens
parse' parsers s tokens =
	let tup = (head parsers) s in
	case (fst tup) of
		Nothing -> parse' (tail parsers) s tokens
		Just x -> parse' parseFunctions (snd tup) (x : tokens)

{--
to do:  archive old, purge old
--}