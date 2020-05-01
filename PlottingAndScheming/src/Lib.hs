{-
André van Meulebrouck
scheme.hs
2018-08-23:  resumed prototype (used for guiding F# implementation) after termination from Smartronix (8/20/2019)
-}

module Lib where
        
import Text.Regex.Posix
import Test.HUnit
import Data.List
-- import Data.Aeson
-- import Test.HSpec

-- to do:  TokComment, TokCommwentBlockStart, TokCommentBlockEnd, TokCrLf

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
    SingleQuote |
    TokString String
    deriving (Eq, Show)

{-
type scmToken =
    | LeftParen
    | RightParen
    | SingleQuote
    | Comment of string    
    | Symbol of string
    //| Complex of complex
    | Float of float
    | Int of int
    | Whitespace of string
    | Dot
    | String of string
    | Bool of char
    //| Sharp of string //to do
    //| BlockComment of string
-}

{-
type scmAtom = 
    | String of string
    | Symbol of string
    | Primitive of (scmCons -> scmBlock option -> scmObject)
    | Complex of complex
    | Float of float
    | Sharp of string
    | Int of int
    //| Bool of bool
and scmBlockType = 
    | Lambda
    | Let
and scmBlock = {
    block : scmCons;
    blockType : scmBlockType;
    mutable parent : scmBlock option; //to do:  discriminated union block + env
    mutable env : scmEnv option; } with
    static member create block blockType = {
        block = block;
        blockType = blockType;
        parent = None;
        env = None; }
    member x.clone () = {
        block = x.block;
        blockType = x.blockType;
        parent = x.parent;
        env = None; }
    member x.toXml () = 
        //to do:  block (call printheap), parent
        let xml = new XElement (xname "scmBlock")
        xml.Add (new XElement ((xname "block"), scmObject.printHeap (scmObject.Cons x.block)))   
        let xmlBlockType =
            match x.blockType with
            | scmBlockType.Lambda -> "lambda"
            | scmBlockType.Let -> "let"
        xml.Add (new XElement ((xname "blockType"), xmlBlockType))        
        let xmlEnv = 
            match x.env with
            | None -> new XElement (xname "env")
            | Some x -> x.toXml ()
        xml.Add (new XElement ((xname "env"), xmlEnv))
        let xmlParent =
            match x.parent with
            | None -> new XElement (xname "parent")
            | Some x -> x.toXml ()
        xml.Add (new XElement ((xname "parent"), xmlParent))            
        xml
and scmThunk = {
    mutable parent : scmBlock option;
    mutable evaled : bool;
    mutable value : scmObject; } with
    static member create parent value = { 
        parent = parent;
        evaled = false;
        value = value; }
and scmObject = 
    | Atom of scmAtom
    | Block of scmBlock
    | Cons of scmCons
    | Thunk of scmThunk
    static member printHeap (heap : scmObject) : string =
        let rec iter heap =
            match heap with
            | scmObject.Atom a ->
                match a with 
                | scmAtom.Symbol s ->
                    s
                | scmAtom.Float f ->
                    f.ToString ()
                | scmAtom.Int i ->
                    i.ToString ()
                | scmAtom.Complex c ->
                    c.ToString ()
                | scmAtom.String s ->
                    s
                | scmAtom.Sharp s ->
                    "#" + s
                | scmAtom.Primitive p ->           
                    "primitive function:  " + p.ToString ()
            | scmObject.Thunk t ->
                let t = t.value
                iter t
            | scmObject.Block b ->
                match b.blockType with 
                | scmBlockType.Lambda ->
                    "lambda"
                | scmBlockType.Let -> 
                    "let"
            | scmObject.Cons c ->
                //walk across a list
                let mutable s = "("
                let mutable cell = c 
                let mutable keepGoing = true
                let mutable firstCell = true
                while keepGoing do
                    match cell.car, cell.cdr with
                    | None, None ->
                        s <- s + ")"
                        keepGoing <- false
                    | Some h, Some t ->
                        match t with
                        | scmObject.Atom _ ->                            
                            s <- s + (iter h) + " . " + (iter t) + ")"
                            keepGoing <- false
                        | scmObject.Block b ->
                            failwith "not implemented yet"
                        | scmObject.Cons c ->
                            if not firstCell then 
                                s <- s + " "
                            s <- s + (iter h) //+ " " 
                            cell <- c
                        | scmObject.Thunk t ->
                            let t = t.value
                            s <- s + (iter t)
                    | Some h, None ->
                        if not firstCell then 
                            s <- s + " "
                        s <- s + (iter h) + ")"
                        keepGoing <- false
                    | None, _ ->
                        failwith "this should never happen:  bad list"
                    firstCell <- false
                s
        iter heap
and scmCons = {
    mutable car : scmObject option;
    mutable cdr : scmObject option; } with
    static member create () = 
        { car = None; cdr = None; }
and bindingNew = {
    symbol : scmAtom;
    mutable thunk : scmThunk; } with
    member x.toXml () = 
        let xml = new XElement ((xname "symbol"), x.symbol)
        xml
and scmEnv = 
    { mutable bindings : bindingNew list } with
    static member create () : scmEnv = { bindings = [] }
    member x.print () =
        x.bindings.Length
    member x.toXml () = //to do
        let root = new XElement (xname "env")
        let rec iter (lst : bindingNew list) =
            match lst with
            | [] -> root
            | h::t -> 
                let sym = 
                    match h.symbol with
                    | scmAtom.Symbol x | scmAtom.String x -> x
                    | scmAtom.Complex x -> x.ToString ()
                    | scmAtom.Int x -> x.ToString ()
                    | scmAtom.Float x -> x.ToString ()
                    | scmAtom.Primitive x -> x.ToString ()
                    | scmAtom.Sharp x -> x.ToString ()                  
                let el = new XElement ((xname "symbol"), sym)
                root.Add el
                iter t
        let xml = iter x.bindings
        xml
    member x.add symbol value evaled (stack : scmStack) = 
        let parent = stack.head
        let thunk = scmThunk.create parent value
        let bindingNew = { symbol = symbol; thunk = thunk }
        x.bindings <- bindingNew :: x.bindings
        ()
    member x.tryFind sym =
        List.tryFind
            (fun b ->
                match b with
                | {symbol = s; thunk = v} ->
                    match s with
                    | scmAtom.Symbol s ->
                        s = sym
                    | _ -> failwith "bad symbol in binding")
            x.bindings   
and symTable = { mutable symbols : scmAtom list } with
    static member create = { symbols = [] }
    member x.getSymbol name =
        let res =
            List.tryFind 
                (fun n -> 
                    match n with
                    | scmAtom.Symbol s -> s = name
                    | _ -> false)
                x.symbols
        match res with
        | None -> 
            let sym = scmAtom.Symbol name
            x.symbols <- sym :: x.symbols
            sym
        | Some s -> 
            s
    member x.tryFind name = 
        List.tryFind 
            (fun n -> 
                match n with
                | scmAtom.Symbol s -> s = name
                | _ -> false)
            x.symbols
and scmStack = {
    mutable frames : scmBlock list; } with 
    static member create () = {
        frames = []; }
    member x.head =
        match x.frames with
        | [] -> None
        | h :: t -> Some h
    member x.add frame =
        x.frames <- frame :: x.frames
    member x.drop =
        x.frames <- (List.tail x.frames)
    member x.pop n =
        for i = 1 to n do
            x.frames <- (List.tail x.frames)
    member x.toXml () = //to do
        ()
-}

-- type scmAtom = 
--     | String of string
--     | Symbol of string
--     | Primitive of (scmCons -> scmBlock option -> scmObject)
--     | Complex of complex
--     | Float of float
--     | Sharp of string
--     | Int of int
--     //| Bool of bool

data ScmAtom =
    AtmString String |
    AtmSymbol String |
    --Primitive :: scmCons -> Maybe scmBlock -> scmObject
    AtmFloat Float |
    -- AtmComplex complex
    AtmSharp String |
    AtmInt Int
    deriving (Eq, Show)

-- and scmBlockType = 
--     | Lambda
--     | Let

data ScmBlockType =
    Let |
    Lambda 
    deriving (Eq, Show)

-- and bindingNew = {
--     symbol : scmAtom;
--     mutable thunk : scmThunk; } with
--     member x.toXml () = 
--         let xml = new XElement ((xname "symbol"), x.symbol)
--         xml

data BindingNew = BindingNew
    { bndSymbol :: ScmAtom
    , bndThunk :: ScmThunk }
    deriving (Eq, Show)

-- and scmEnv = 
--     { mutable bindings : bindingNew list } with
--     static member create () : scmEnv = { bindings = [] }
--     member x.print () =
--         x.bindings.Length
--     member x.toXml () = //to do
--         let root = new XElement (xname "env")
--         let rec iter (lst : bindingNew list) =
--             match lst with
--             | [] -> root
--             | h::t -> 
--                 let sym = 
--                     match h.symbol with
--                     | scmAtom.Symbol x | scmAtom.String x -> x
--                     | scmAtom.Complex x -> x.ToString ()
--                     | scmAtom.Int x -> x.ToString ()
--                     | scmAtom.Float x -> x.ToString ()
--                     | scmAtom.Primitive x -> x.ToString ()
--                     | scmAtom.Sharp x -> x.ToString ()                  
--                 let el = new XElement ((xname "symbol"), sym)
--                 root.Add el
--                 iter t
--         let xml = iter x.bindings
--         xml
--     member x.add symbol value evaled (stack : scmStack) = 
--         let parent = stack.head
--         let thunk = scmThunk.create parent value
--         let bindingNew = { symbol = symbol; thunk = thunk }
--         x.bindings <- bindingNew :: x.bindings
--         ()
--     member x.tryFind sym =
--         List.tryFind
--             (fun b ->
--                 match b with
--                 | {symbol = s; thunk = v} ->
--                     match s with
--                     | scmAtom.Symbol s ->
--                         s = sym
--                     | _ -> failwith "bad symbol in binding")
--             x.bindings   

-- data ScmEnv = ScmEnv 
--     { envBindings :: [] }

scmEnv :: [BindingNew] -> [BindingNew] -> [BindingNew]
scmEnv = (++) 

-- and scmCons = {
--     mutable car : scmObject option;
--     mutable cdr : scmObject option; } with
--     static member create () = 
--         { car = None; cdr = None; }

data ScmCons = ScmCons --done
    { scmCar :: Maybe ScmObject
    , scmCdr :: Maybe ScmObject }
    deriving (Eq, Show)

-- and scmBlock = {
--     block : scmCons;
--     blockType : scmBlockType;
--     mutable parent : scmBlock option; //to do:  discriminated union block + env
--     mutable env : scmEnv option; } with
--     static member create block blockType = {
--         block = block;
--         blockType = blockType;
--         parent = None;
--         env = None; }
--     member x.clone () = {
--         block = x.block;
--         blockType = x.blockType;
--         parent = x.parent;
--         env = None; }
--     member x.toXml () = 
--         //to do:  block (call printheap), parent
--         let xml = new XElement (xname "scmBlock")
--         xml.Add (new XElement ((xname "block"), scmObject.printHeap (scmObject.Cons x.block)))   
--         let xmlBlockType =
--             match x.blockType with
--             | scmBlockType.Lambda -> "lambda"
--             | scmBlockType.Let -> "let"
--         xml.Add (new XElement ((xname "blockType"), xmlBlockType))        
--         let xmlEnv = 
--             match x.env with
--             | None -> new XElement (xname "env")
--             | Some x -> x.toXml ()
--         xml.Add (new XElement ((xname "env"), xmlEnv))
--         let xmlParent =
--             match x.parent with
--             | None -> new XElement (xname "parent")
--             | Some x -> x.toXml ()
--         xml.Add (new XElement ((xname "parent"), xmlParent))            
--         xml

data ScmBlock = ScmBlock
    { blkCons :: ScmCons
    , blkType :: ScmBlockType 
    , blkParent :: Maybe ScmBlock
    , blkEnv :: Maybe [BindingNew] --possibly use empty list rather than Maybe
    } deriving (Eq, Show)

-- and scmObject = 
--     | Atom of scmAtom
--     | Block of scmBlock
--     | Cons of scmCons
--     | Thunk of scmThunk
--     static member printHeap (heap : scmObject) : string =
--         let rec iter heap =
--             match heap with
--             | scmObject.Atom a ->
--                 match a with 
--                 | scmAtom.Symbol s ->
--                     s
--                 | scmAtom.Float f ->
--                     f.ToString ()
--                 | scmAtom.Int i ->
--                     i.ToString ()
--                 | scmAtom.Complex c ->
--                     c.ToString ()
--                 | scmAtom.String s ->
--                     s
--                 | scmAtom.Sharp s ->
--                     "#" + s
--                 | scmAtom.Primitive p ->           
--                     "primitive function:  " + p.ToString ()
--             | scmObject.Thunk t ->
--                 let t = t.value
--                 iter t
--             | scmObject.Block b ->
--                 match b.blockType with 
--                 | scmBlockType.Lambda ->
--                     "lambda"
--                 | scmBlockType.Let -> 
--                     "let"
--             | scmObject.Cons c ->
--                 //walk across a list
--                 let mutable s = "("
--                 let mutable cell = c 
--                 let mutable keepGoing = true
--                 let mutable firstCell = true
--                 while keepGoing do
--                     match cell.car, cell.cdr with
--                     | None, None ->
--                         s <- s + ")"
--                         keepGoing <- false
--                     | Some h, Some t ->
--                         match t with
--                         | scmObject.Atom _ ->                            
--                             s <- s + (iter h) + " . " + (iter t) + ")"
--                             keepGoing <- false
--                         | scmObject.Block b ->
--                             failwith "not implemented yet"
--                         | scmObject.Cons c ->
--                             if not firstCell then 
--                                 s <- s + " "
--                             s <- s + (iter h) //+ " " 
--                             cell <- c
--                         | scmObject.Thunk t ->
--                             let t = t.value
--                             s <- s + (iter t)
--                     | Some h, None ->
--                         if not firstCell then 
--                             s <- s + " "
--                         s <- s + (iter h) + ")"
--                         keepGoing <- false
--                     | None, _ ->
--                         failwith "this should never happen:  bad list"
--                     firstCell <- false
--                 s
--         iter heap

--     | Atom of scmAtom
--     | Block of scmBlock
--     | Cons of scmCons
--     | Thunk of scmThunk

data ScmObject =
    ObjAtom ScmAtom |
    ObjBlock ScmBlock |
    ObjCons ScmCons |
    ObjThunk ScmThunk
    deriving (Eq, Show)   

-- and symTable = { mutable symbols : scmAtom list } with
--     static member create = { symbols = [] }
--     member x.getSymbol name =
--         let res =
--             List.tryFind 
--                 (fun n -> 
--                     match n with
--                     | scmAtom.Symbol s -> s = name
--                     | _ -> false)
--                 x.symbols
--         match res with
--         | None -> 
--             let sym = scmAtom.Symbol name
--             x.symbols <- sym :: x.symbols
--             sym
--         | Some s -> 
--             s
--     member x.tryFind name = 
--         List.tryFind 
--             (fun n -> 
--                 match n with
--                 | scmAtom.Symbol s -> s = name
--                 | _ -> false)
--             x.symbols

-- data SymTable =

-- and scmStack = {
--     mutable frames : scmBlock list; } with 
--     static member create () = {
--         frames = []; }
--     member x.head =
--         match x.frames with
--         | [] -> None
--         | h :: t -> Some h
--     member x.add frame =
--         x.frames <- frame :: x.frames
--     member x.drop =
--         x.frames <- (List.tail x.frames)
--     member x.pop n =
--         for i = 1 to n do
--             x.frames <- (List.tail x.frames)
--     member x.toXml () = //to do
--         ()

--one item records vs function?

-- data ScmStack = ScmStack 
--     { stkFrames :: [ScmBlock] } 

scmStack :: ScmBlock -> [ScmBlock] -> [ScmBlock]
scmStack = (:)

-- data ScmStack :: [ScmBlock]
-- ScmStack = []

-- and scmThunk = {
--     mutable parent : scmBlock option;
--     mutable evaled : bool;
--     mutable value : scmObject; } with
--     static member create parent value = { 
--         parent = parent;
--         evaled = false;
--         value = value; }

data ScmThunk = ScmThunk 
    { thkParent :: Maybe ScmBlock }
    deriving (Eq, Show)

-- data Ast =
--     AstString String |
--     AstCommentLine String |
--     AstCommentBlock String
--     deriving (Eq, Show)

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

tokParseLeftParen :: String ->Either String (Maybe Token, String)
tokParseLeftParen s = Right $ parseLeftParen s

parseRightParen :: String -> (Maybe Token, String)
parseRightParen [] = (Nothing, [])
parseRightParen s =
    if head s == ')'
    then (Just RightParen, tail s)
    else (Nothing, s)

tokParseRightParen :: String -> Either String (Maybe Token, String)
tokParseRightParen s = Right $ parseRightParen s

parseSingleQuote :: String -> (Maybe Token, String)
parseSingleQuote [] = (Nothing, [])
parseSingleQuote s =
    if head s == '\''
    then (Just SingleQuote, tail s)
    else (Nothing, s)

tokParseSingleQuote :: String -> Either String (Maybe Token, String)
tokParseSingleQuote s = Right $ parseSingleQuote s

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

tokParseAtom :: String -> Either String (Maybe Token, String)
tokParseAtom s = Right $ parseAtom s

parseWhitespace :: String -> (Maybe Token, String)
parseWhitespace [] = (Nothing, [])
parseWhitespace s = 
    let sym = takeWhile isWhitespace s in
    if sym == "" then (Nothing, s)
    else (Just (Whitespace sym), drop (length sym) s)

tokParseWhitespace :: String -> Either String (Maybe Token, String)
tokParseWhitespace s = Right $ parseWhitespace s
    
parseComment :: String -> (Maybe Token, String)
parseComment [] = (Nothing, [])
parseComment s = 
    if head s == ';' then
        let tok = takeWhile (\x -> x /= '\n') (tail s) in
        (Just (Comment tok), drop ((length tok) + 1) s)
    else
        (Nothing, s)

tokParseComment :: String -> Either String (Maybe Token, String)
tokParseComment s = Right $ parseComment s       

takeFromWhile :: String -> (Char -> Bool) -> (String, String)
takeFromWhile s p = iter s [] where
    iter s acc
        | s == [] = (reverse acc, [])
        | p $ head s = iter (tail s) ((head s) : acc)
        | otherwise = (reverse acc, s)

parseString :: String -> Either String (Maybe Token, String)
parseString [] = Right (Nothing, [])
parseString s = 
    if head s == '\"' then 
        let close = elemIndex '\"' $ tail s in
            case close of
                Nothing -> Left "unterminated string"
                Just x -> Right (Just $ TokString $ take x $ drop 1 $ s, drop (x + 2) s)
        -- let (str, rst) = takeFromWhile (tail s) (/='"') 
        -- in
        --     if rst == [] then Left "unterminated string"
        --     else Right (Just $ TokString str, tail rst)
    else Right (Nothing, s)

parseFunctions :: [String -> (Maybe Token, String)]
parseFunctions = 
    [parseComment
    ,parseLeftParen
    ,parseRightParen
    ,parseSingleQuote
    ,parseWhitespace
    ,parseAtom
    ]

tokParseFunctions :: [String -> Either String (Maybe Token, String)]
tokParseFunctions = 
    [ tokParseComment
    , tokParseLeftParen
    , tokParseRightParen
    , tokParseSingleQuote
    , tokParseWhitespace
    , tokParseAtom
    , parseString ]

-- parse :: String -> [Token]
-- parse [] = []
-- parse s = parse' parseFunctions s [] where
--     parse' :: [String -> (Maybe Token, String)] -> String -> [Token] -> [Token]
--     parse' [] s tokens = error $ "parse:  unparsable expression:  " ++ s
--     parse' parsers [] tokens = reverse tokens
--     parse' parsers s tokens =
--         let tup = (head parsers) s in
--         case (fst tup) of
--             Nothing -> parse' (tail parsers) s tokens
--             Just x -> parse' parseFunctions (snd tup) (x : tokens)

tokParse :: String -> Either String [Token]
tokParse [] = Right []
tokParse s = parse' tokParseFunctions s [] where
    parse' :: [String -> Either String (Maybe Token, String)] -> String -> [Token] -> Either String [Token]
    parse' [] s tokens = Left $ "tokParse:  unparsable expression:  " ++ s --should this just return tokens?
    parse' parsers [] tokens = Right $ reverse tokens
    parse' parsers s tokens = --undefined
        let res = (head parsers) s in
            case res of
                Right (Nothing, y) -> parse' (tail parsers) y tokens
                Right (Just x, y) -> parse' tokParseFunctions y (x : tokens)
                Left x -> Left x

{-
need test for unterminated string
need to source control on my github account
need to create function for paren checking using fold and returns a tuple (current, highest, lefts, rights, errorMaybe)
need backup
-}

parseTests = 
    [ ("car", [Symbol "car"])
    , ("(car '(a b c))", [LeftParen,Symbol "car",Whitespace " ",SingleQuote,LeftParen,Symbol "a",Whitespace " ",Symbol "b",Whitespace " ",Symbol "c",RightParen,RightParen])
    , ("(car '(a b . c))", [LeftParen,Symbol "car",Whitespace " ",SingleQuote,LeftParen,Symbol "a",Whitespace " ",Symbol "b",Whitespace " ",Dot,Whitespace " ",Symbol "c",RightParen,RightParen])
    , ("(car '(234324fsdfds-sdfdsfsdf3.5))", [LeftParen,Symbol "car",Whitespace " ",SingleQuote,LeftParen,Symbol "234324fsdfds-sdfdsfsdf3.5",RightParen,RightParen])
    , ("(car (cdr (cons '(-3. . +5) (cons 4.5 23423432))))", 
        [LeftParen
        ,Symbol "car"
        ,Whitespace " "
        ,LeftParen,Symbol "cdr"
        ,Whitespace " "
        ,LeftParen
        ,Symbol "cons"
        ,Whitespace " "
        ,SingleQuote
        ,LeftParen,Float "-3."
        ,Whitespace " "
        ,Dot,Whitespace " "
        ,Integer "+5"
        ,RightParen
        ,Whitespace " "
        ,LeftParen
        ,Symbol "cons"
        ,Whitespace " "
        ,Float "4.5"
        ,Whitespace " "
        ,Integer "23423432"
        ,RightParen
        ,RightParen
        ,RightParen
        ,RightParen])
    , ( "(car '(\"a\" \"b\" \"c\"))"
      , [LeftParen, Symbol "car", Whitespace " ", SingleQuote, LeftParen, TokString "a", Whitespace " ", TokString "b", Whitespace " ", TokString "c", RightParen, RightParen])
    , ( "(car '(\"sdfdsfds\" 3 arsd\"sd()fdsf\"))"
      , [LeftParen,Symbol "car",Whitespace " ",SingleQuote,LeftParen,TokString "sdfdsfds",Whitespace " ",Integer "3",Whitespace " ",Symbol "arsd",TokString "sd()fdsf",RightParen,RightParen])
    , ( "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsfdsf()dfssdf)\" abc(sd sd d)))"
      , [LeftParen,Symbol "define",Whitespace " ",Symbol "foo",SingleQuote,LeftParen,Symbol "sdfsdf.sdfsdfds",Whitespace " ",Symbol "sdfds",TokString "dsfdsf",Whitespace " ",Integer "3",Whitespace " ",Integer "-3",Whitespace " ",TokString "fdsfdsf()dfssdf)",Whitespace " ",Symbol "abc",LeftParen,Symbol "sd",Whitespace " ",Symbol "sd",Whitespace " ",Symbol "d",RightParen,RightParen,RightParen])
    , ( "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsf'dsf()dfssdf)\" abc(sd sd d)))"
      , [LeftParen,Symbol "define",Whitespace " ",Symbol "foo",SingleQuote,LeftParen,Symbol "sdfsdf.sdfsdfds",Whitespace " ",Symbol "sdfds",TokString "dsfdsf",Whitespace " ",Integer "3",Whitespace " ",Integer "-3",Whitespace " ",TokString "fdsf'dsf()dfssdf)",Whitespace " ",Symbol "abc",LeftParen,Symbol "sd",Whitespace " ",Symbol "sd",Whitespace " ",Symbol "d",RightParen,RightParen,RightParen])
    ]

-- assertBool :: String -> Bool -> Assertion
-- assertBool msg b = unless b (assertFailure msg)

testEmpty = TestCase $ assertEqual 
  "Should get Nothing from an empty string" Nothing ( Just "5" ) 

-- parseTest :: [(String, [Token])] -> [((String, [Token]), [Token])]
-- parseTest [] = []
-- parseTest p = foldl (\acc x -> let res = parse (fst x) in if res == (snd x) then acc else ((x, res) : acc)) [] p

parseTest' :: [(String, [Token])] -> [((String, [Token]), [Token])]
parseTest' [] = []
parseTest' p = foldl (\acc x -> 
    let res = tokParse (fst x) in 
        case res of
            Right r ->
                if r == (snd x) then acc else ((x, r) : acc)
            Left l ->
                ((x, []) : acc)) [] p

someFunc :: IO ()
someFunc = do 
    -- putStrLn $ show $ tokParse "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsf'dsf()dfssdf)\" abc(sd sd d)))"
    -- putStrLn $ show $ elemIndex 'a' "cat"
    -- putStrLn $ show $ parseFloat "-3."
    -- putStrLn $ show $ parseInteger "33"
    -- putStrLn $ show $ parseLeftParen "("
    -- putStrLn $ show $ parseString "\"cat\""
    -- putStrLn $ show $ tokParse "(car '(\"a\"))"
    --runTestTT testEmpty
--     putStrLn $ show $ parse ";ok\n\
-- \(let ((TRUE (lambda (x) (lambda (y) x)))\
--       \(FALSE (lambda (x) (lambda (y) y))))\
--   \(let ((IF (lambda (p) (lambda (t) (lambda (e) (((p t) e)))))))\
--         \(((IF FALSE) (lambda () 1)) (lambda () 2))))"
    -- let foo = tester parseTests in
    -- -- assertBool "It should be true" True
    -- -- assertBool "It should be true" False
    -- -- assertBool "It should be true (again)" False
    -- -- testEmpty
    -- -- putStrLn (p1)
    -- -- assertBool "test 1" ((parse "(car '(234324fsdfds-sdfdsfsdf3.5))") == "(car '(234324fsdfds-sdfdsfsdf3.5))")
    -- -- putStrLn $ show p1
    -- -- assertBool "test1" $ p1 == [LeftParen,Symbol "car",Whitespace " ",SingleQuote,LeftParen,Symbol "a",Whitespace " ",Symbol "b",Whitespace " ",Symbol "c",RightParen,RightParen]
    -- -- putStrLn $ show $ parse "car"
    -- -- putStrLn $ show p11
    -- -- putStrLn $ show "hey \"you\""
    -- -- putStrLn $ show $ Symbol "car" == (Symbol "car")
    -- -- putStrLn $ show $ (show p0) == "[Symbol \"car\"]"
    -- -- assertBool "test1" $ s1 == "[LeftParen, Symbol \"car\"]"
    -- -- putStrLn $ show [Symbol "car", Symbol "cdr"]
    -- -- assertBool "test0" $ p0 == [Symbol "car"]
    -- -- assertBool "test11" $ p11 == [LeftParen,Symbol "car",Whitespace " ",SingleQuote,LeftParen,Symbol "234324fsdfds-sdfdsfsdf3.5",RightParen,RightParen]
    -- putStrLn $ show $ parseTest parseTests
    putStrLn $ show $ parseTest' parseTests
    putStrLn ("done")    

{--
to do:  block comments
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