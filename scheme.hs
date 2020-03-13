{-
André van Meulebrouck
scheme.hs
-}

import Text.Regex.Posix
import Control.Applicative

data Token =
	Comment String |
	LeftParen |
	RightParen |
	Dot |
	Symbol String |
	Integer String |
	Float String |
	Whitespace String |
	Literal String |
	SingleQuote 
	deriving (Eq, Show)

symbolChars :: String
symbolChars = 
	['a'..'z'] ++
	['A'..'Z'] ++
	['0'..'9'] ++
	"-_.~>=+&*^%!@#$:?<>/"

whitespaceChars :: String
whitespaceChars = " \r\n\t"

isSymbolChar :: Char -> Bool
isSymbolChar c = elem c symbolChars

isWhitespace :: Char -> Bool
isWhitespace x = elem x whitespaceChars

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
	let m = s =~ "^[+|-]?[[:digit:]]+[.][[:digit:]]*|^[+|-]?[[:digit:]]*[.][[:digit:]]+" :: String in
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
	let sym = takeWhile isSymbolChar s in 
	if sym == "" then (Nothing, s)
	else (Just (Symbol sym), drop (length sym) s)

parseAtom :: String -> (Maybe Token, String)
parseAtom [] = (Nothing, [])
parseAtom s = parseAtom' [parseDot, parseFloat, parseInteger] s

parseAtom' :: [String -> (Maybe Token, String)] -> String -> (Maybe Token, String)
parseAtom' parsers s =
	case parsers of
		[] -> parseSymbol s
		(h:t) -> 
			let tok = h s in
			case fst tok of
				Nothing -> parseAtom' t s
				Just x -> 
					if elem (head (snd tok)) symbolChars then
						parseSymbol s --to do:  don't parse from start
					else
						tok

parseWhitespace :: String -> (Maybe Token, String)
parseWhitespace [] = (Nothing, [])
parseWhitespace s = 
	let sym = takeWhile isWhitespace s in
	if sym == "" then (Nothing, s)
	else (Just (Whitespace sym), drop (length sym) s)

parseComment :: String -> (Maybe Token, String)
parseComment [] = (Nothing, [])
parseComment s = 
	if head s == ';' then
		let tok = takeWhile (\x -> x /= '\n') (tail s) in
		(Just (Comment tok), drop ((length tok) + 1) s)
	else
		(Nothing, s)

parseFunctions :: [String -> (Maybe Token, String)]
parseFunctions = 
	[parseComment
	,parseLeftParen
	,parseRightParen
	,parseSingleQuote
	,parseWhitespace
	,parseAtom
	]

parse :: String -> [Token]
parse [] = []
parse s = parse' parseFunctions s [] where
	parse' :: [String -> (Maybe Token, String)] -> String -> [Token] -> [Token]
	parse' [] s tokens = error $ "unparsable expression:  " ++ s
	parse' parsers [] tokens = reverse tokens
	parse' parsers s tokens =
		let tup = (head parsers) s in
		case (fst tup) of
			Nothing -> parse' (tail parsers) s tokens
			Just x -> parse' parseFunctions (snd tup) (x : tokens)

{--
to do:  block comments, strings
--}

{--
*Main> parse "(car '(234324fsdfds-sdfdsfsdf3.5))"
[LeftParen,Symbol "car",Whitespace " ",SingleQuote,LeftParen,Symbol "234324fsdfds-sdfdsfsdf3.5",RightParen,RightParen]

*Main Text.Regex.Posix> parse "(car (cdr (cons '(-3. . +5) (cons 4.5 23423432))))"
[LeftParen,Symbol "car",Whitespace " ",LeftParen,Symbol "cdr",Whitespace " ",LeftParen,Symbol "cons",Whitespace " ",SingleQuote,LeftParen,Float "-3.",Whitespace " ",Dot,Whitespace " ",Integer "+5",RightParen,Whitespace " ",LeftParen,Symbol "cons",Whitespace " ",Float "4.5",Whitespace " ",Integer "23423432",RightParen,RightParen,RightParen,RightParen]

*Main> tokenize "(car '(\"sdfdsfds\" 3 arsd\"sd()fdsf\"))"
[LeftParen,Atom "car",SingleQuote,LeftParen,Str "sdfdsfds",Whitespace " ",Atom "3",Atom "arsd",Str "sd()fdsf",RightParen,RightParen]

*Main> tokenize "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsfdsf()dfssdf)\" abc(sd sd d)))"
[LeftParen,Atom "define",SingleQuote,Atom "foo",Atom "sdfsdf.sdfsdfds",Atom "sdfds",Str "dsfdsf",Whitespace " ",Atom "3",Atom "-3",Str "fdsfdsf()dfssdf)",Whitespace " ",Atom "abc",Atom "sd",Atom "sd",Atom "d",RightParen,RightParen]

*Main> tokenize "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsf'dsf()dfssdf)\" abc(sd sd d)))"
[LeftParen,Atom "define",Atom "foo",SingleQuote,LeftParen,Atom "sdfsdf.sdfsdfds",Atom "sdfds",Str "dsfdsf",Whitespace " ",Atom "3",Atom "-3",Str "fdsf'dsf()dfssdf)",Whitespace " ",Atom "abc",Atom "sd",Atom "sd",Atom "d",RightParen,RightParen]
--}