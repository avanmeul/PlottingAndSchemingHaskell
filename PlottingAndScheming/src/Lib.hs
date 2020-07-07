{-
André van Meulebrouck
scheme.hs
2019-08-23:  resumed prototype (used for guiding F# implementation) after termination from Smartronix (8/20/2019)
-}

module Lib where

import Text.Regex.Posix
import Text.Read
-- import Test.HUnit
import Data.List
-- import Control.Monad.State.Lazy
import Control.Monad.State
import Data.IORef
-- import Data.Aeson
-- import Test.HSpec
import Text.Show.Functions

--to do:  null?, cons; then define reverse

--to do:  TokComment, TokCommwentBlockStart, TokCommentBlockEnd, TokCrLf

data Token =
    TokComment String |
    TokLeftParen |
    TokRightParen |
    TokDot |
    TokSymbol String |
    TokInteger String |
    TokFloat String |
    TokWhitespace String |
    TokLiteral String |
    TokSingleQuote |
    TokString String
    deriving (Eq, Show)

data ScmImm = 
    ImmSym String |
    ImmRat (Int, Int) |
    ImmInt Int |
    ImmFloat Float |
    ImmComplex (Float, Float) |
    ImmString String
    deriving (Eq, Show)

data ScmPrimitive = ScmPrimitive
    { priName :: String
    , priFunction :: ScmContext -> ScmObject -> Either [ScmError] ScmObject }
    deriving (Show) --to do:  ad Eq

data ScmClosure = ScmClosure
    { clsCtx :: ScmContext
    , clsParameters :: ScmObject
    , clsBody :: ScmObject }
    deriving (Show)

data ScmObject =
    ObjContext ScmContext |
    ObjSymbol String |
    ObjImmediate ScmImm |
    ObjBlock ScmBlock |
    ObjCons ScmCons |
    ObjError String |
    ObjThunk ScmThunk |
    ObjClosure ScmClosure |
    ObjPrimitive ScmPrimitive
    deriving (Show) --to do:  add Eq

data ScmThunk = ScmThunk
    { thkCtx :: ScmContext
    , thkValue :: ScmObject
    , thkEvaled :: Bool }
    deriving (Show) --to do:  add Eq

getToken :: [Token] -> (Maybe Token, [Token])
getToken [] = (Nothing, [])
getToken ((TokComment _) : t) = getToken t
getToken ((TokWhitespace _) : t) = getToken t
getToken (h : t) = (Just h, t)

-- type Stack = [Int]  
  
-- test :: State Int Int
-- test = do
--   put 3
--   modify (+1)
--   Control.Monad.State.get

symNil :: ScmObject
symNil = ObjImmediate $ ImmSym "()"

symTrue :: ScmObject
symTrue = ObjImmediate $ ImmSym "#t"

symFalse :: ScmObject
symFalse = ObjImmediate $ ImmSym "#f"

data ScmCons = ScmCons
    { scmCar :: ScmObject
    , scmCdr :: ScmObject }
    deriving (Show) --to do:  ad Eq

addToCons :: ScmCons -> [ScmObject] -> Maybe ScmCons
addToCons cons [] = Just cons
addToCons cons (h : t) = 
    let next =  ScmCons { scmCar = h, scmCdr = ObjCons cons } 
    in addToCons next t

createCons :: [ScmObject] -> Maybe ScmCons
createCons [] = Nothing
createCons (h : t) = 
    let cons = ScmCons { scmCar = h, scmCdr = symNil } 
    in addToCons cons t

createPair :: [ScmObject] -> Maybe ScmCons
createPair [] = Nothing
createPair (h : []) = Nothing
createPair (h1 : h2 : t) = 
    let cons = ScmCons { scmCar = h2, scmCdr = h1 } 
    in addToCons cons t

tokNonSemantic :: Token -> Bool
tokNonSemantic (TokWhitespace _) = True
tokNonSemantic (TokComment _) = True
tokNonSemantic _ = False

toksRemoveNonSemantic :: [Token] -> [Token]
toksRemoveNonSemantic x = 
    filter (\x -> not $ tokNonSemantic x) x
  
buildHeap :: [Token] -> Either (String, [Token]) (ScmObject, [Token]) --to do:  should be ScmError rather than String
buildHeap [] = 
    Left ("out of tokens", [])
buildHeap ((TokSymbol i@('#' : s)) : t) = 
    Right (ObjImmediate (ImmSym i), t)
buildHeap ((TokSymbol x) : t) =
    Right (ObjSymbol x, t)
buildHeap (TokSingleQuote : t) =
    case (buildHeap t) of 
        Left (m, t) -> Left (m, t)
        Right (x, t) -> 
            case (createCons [x, ObjSymbol "quote"]) of
                Nothing -> Left ("buildHeap:  failed to create object of a quote", t)
                Just x -> Right (ObjCons x, t)
buildHeap ((TokString x) : t) =
    Right (ObjImmediate $ ImmString x, t)
buildHeap ((TokInteger x) : t) =
    case (readMaybe x :: Maybe Int) of
        Just i -> Right (ObjImmediate $ ImmInt i, t)
        Nothing -> Right (ObjError $ "buildHeap:  parse fail on int:  " ++ x, t)
buildHeap ((TokFloat x) : t) =
    case (readMaybe x :: Maybe Float) of
        Just f -> Right (ObjImmediate $ ImmFloat f, t)
        Nothing -> Right (ObjError $ "buildHeap:  parse fail on float:  " ++ x, t)
buildHeap (TokLeftParen : TokRightParen : t) =
    Right (ObjImmediate $ ImmSym "()", t)
buildHeap (TokLeftParen : t) = --walk across top level list until a right paren is discovered
    let res = iter t [] where
        iter :: [Token] -> [ScmObject] -> Either (String, [Token]) (ScmCons, [Token])
        iter [] lst =
            Left ("out of tokens", [])
        iter (TokRightParen : t) lst =
            let res = createCons lst in
                case (res) of
                    Nothing -> Left ("buildHeap:  failure to create cons cells, site 1", t)
                    Just x -> Right (x, t)
        iter (TokDot : t) lst = 
            let cdr = buildHeap t in
                case (cdr) of 
                    Right (o, TokRightParen : rst) -> 
                        let res = createPair (o : lst) in
                            case (res) of 
                                Nothing -> Left ("buildHeap:  failure to create dotted pair", rst)
                                Just x -> Right (x, rst)
                    otherwise ->
                        Left ("buildHeap:  bad tail in dotted pair", t)
        iter toks lst =
            case (buildHeap toks) of
                Left x -> Left x
                Right (o, t) -> iter t (o : lst)
    in
        case (res) of
            Left (e, t) -> Left ("buildHeap:  failed to create cons cells, site 2", t)
            Right (x, t) -> Right (ObjCons x, t)
buildHeap (TokRightParen : t) = --if this happens, it indicates that a right occurred without a prior left, i.e. )(
    Left ("buildHeap:  right paren before left", t)
buildHeap (TokWhitespace x : t) = 
    buildHeap t --this is just in case these aren't filtered out before calling buildHeap
buildHeap (TokComment x : t) = 
    buildHeap t --this is just in case these aren't filtered out before calling buildHeap
buildHeap tokens = --this should never happen
    Left ("not implemented", tokens)

printHeap :: ScmObject -> String
printHeap (ObjSymbol x) = x
printHeap (ObjImmediate (ImmSym x)) = x
printHeap (ObjImmediate (ImmInt x)) = show x
printHeap (ObjImmediate (ImmFloat x)) = show x
printHeap (ObjImmediate (ImmString x)) = "\"" ++ x ++ "\""
printHeap (ObjImmediate _) = "#<error:  unknown immediate>"
printHeap (ObjPrimitive (ScmPrimitive { priName = nm, priFunction = _ })) = "#<primitive " ++ nm ++ ">"
printHeap (ObjClosure x) = "#<closure>"
printHeap (ObjThunk x) = "#<thunk>"
printHeap (ObjError x) = "#<error>"
printHeap (ObjCons x) = concat $ reverse $ iter ["("] $ ObjCons x where
    iter :: [String] -> ScmObject -> [String]
    iter lst (ObjCons (ScmCons { scmCar = h, scmCdr = (ObjImmediate (ImmSym "()")) })) = 
        ")" : printHeap h : lst
    iter lst (ObjCons (ScmCons { scmCar = h, scmCdr = (ObjCons t) })) = --cdr has more list elements
        iter (" " : (printHeap h) : lst) (ObjCons t)
    iter lst (ObjCons (ScmCons { scmCar = h, scmCdr = t })) = --cdr isn't cons and isn't ()
        ")" : (printHeap t) : " . " : (printHeap h) : lst                              
printHeap (ObjContext x) = "#<context>"
printHeap _ = "#<unknown object type>"

-- fac :: Int -> Int
-- fac 0 = 1
-- fac n = n * (fac $ n - 1)

{-
Replace thunks with result objects (which are built from ScmObject but are evaluated to a result; hence like hidden quote functions).

Haskell LISP equivalents of LISP:

cnsCons
cnsHead
cnsTail
cnsNil
-}

scmQuote :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmQuote ctx (ObjCons ScmCons { scmCar = h, scmCdr = ObjImmediate (ImmSym "()") }) = Right h
scmQuote _ _ = Left [ ScmError { errCaller = "scmQuote", errMessage = "bad arg" } ]

scmHead :: ScmContext -> ScmObject -> Either [ScmError] ScmObject --to do:  refactor case to equations
scmHead ctx (ObjCons ScmCons { scmCar = h, scmCdr = ObjImmediate (ImmSym "()") }) =
    case (eval ctx h) of
        Right (ObjCons (ScmCons { scmCar = h, scmCdr = _ })) -> Right h
        otherwise -> Left [ ScmError { errCaller = "head (site 1)", errMessage = "bad arg" } ]
scmHead _ _ = Left [ ScmError { errCaller = "head (site 2)", errMessage = "bad arg" } ]

scmTail :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmTail ctx (ObjCons (ScmCons { scmCar = h, scmCdr = ObjImmediate (ImmSym "()") })) = 
    case (eval ctx h) of
        Right (ObjCons (ScmCons { scmCar = _, scmCdr = t })) -> Right t
        otherwise -> Left [ ScmError { errCaller = "tail (site 1)", errMessage = "bad arg" } ]
scmTail _ _ = Left [ ScmError { errCaller = "tail (site 2)", errMessage = "bad arg" } ]

scmDefine :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmDefine ctx args = 
    let arg = Just args
        sym = safeCar arg
        obj = safeCar $ safeCdr arg
    in 
        case (sym, obj) of
            (Just (ObjSymbol s), Just o) ->
                let thunks = thunkifyArgList ctx [o]
                    env = filter (\ (l, v) -> l /= s) $ ctxEnv ctx --this needs to be the extended environment
                in 
                    if (length thunks == 1 && (null $ ctxStk ctx)) then
                        Right $ ObjContext (ScmContext { ctxEnv = (s, (head thunks)) : env, ctxStk = [] })
                    else 
                        Left [ScmError { errCaller = "scmDefine", errMessage = "too many args or not at top level" }]
            otherwise -> 
                Left [ScmError { errCaller = "scmDefine", errMessage = "arguments to define were invalid" }]

scmIf :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmIf ctx args =
    let arg = Just args
        cdr1 = safeCdr arg
        cdr2 = safeCdr cdr1
        cdr3 = safeCdr cdr2
        predicate = safeCar arg
        thenClause = safeCar cdr1
        elseClause = safeCar cdr2
    in
        case (predicate, thenClause, elseClause, cdr3) of
            (Just p, Just t, Just e, Just (ObjImmediate (ImmSym "()"))) -> 
                case (eval ctx p) of
                    Right (ObjImmediate (ImmSym "#t")) ->
                        eval ctx t
                    Right (ObjImmediate (ImmSym "#f")) ->
                        eval ctx e
                    Right x ->
                        Left [ScmError { errCaller = "scmIf", errMessage = "invalid predicate:  " ++ (show x) }]
                    Left x -> 
                        Left $ ScmError { errCaller = "scmIf", errMessage = "predicate evaluation failed:  " ++ (show p) } : x
            otherwise -> 
                let msg = "invalid if:  predicate = " ++ (show predicate) ++ " then = " ++ (show thenClause) ++ ", else = " ++ (show elseClause) in
                    Left [ScmError { errCaller = "scmIf", errMessage = msg }]

scmZero :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmZero ctx args = 
    let arg = safeCar $ Just args
    in 
        case arg of
            Just x -> 
                case (eval ctx x) of
                    Right (ObjImmediate (ImmInt x)) ->
                        if x == 0 then
                            Right symTrue
                        else 
                            Right symFalse
                    Right (ObjImmediate (ImmFloat x)) -> 
                        if x == 0.0 then
                            Right symTrue
                        else 
                            Right symFalse
                    Left x ->
                        Left $ ScmError { errCaller = "scmZero", errMessage = "bad argument:  " ++ (show x) } : x
                    otherwise -> 
                        Left [ScmError { errCaller = "scmZero", errMessage = "bad argument:  " ++ (show x) }]
            Nothing -> 
                Left [ScmError { errCaller = "scmZero", errMessage = "bad argument:  " ++ (show arg) }]

scmPlus :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmPlus ctx args = 
    case (cnsToList args) of
        Just x -> 
            iter x (Just 0, Just 0.0) where
                iter :: [ScmObject] -> (Maybe Int, Maybe Float) -> Either [ScmError] ScmObject
                iter [] (Just i, Just f) = Right $ ObjImmediate $ ImmInt i
                iter [] (Nothing, Just x) = Right $ ObjImmediate $ ImmFloat x
                iter (h : t) (sumi, Just f) = 
                    case (eval ctx h) of
                        Right (ObjImmediate (ImmInt x)) -> 
                            case sumi of
                                Just i -> iter t (Just $ i + x, Just $ f + (fromIntegral x))
                                otherwise -> iter t (Nothing, Just $ f + (fromIntegral x))
                        Right (ObjImmediate (ImmFloat x)) -> 
                            iter t (Nothing, Just $ f + x)
                        Right x -> 
                            Left [ScmError { errCaller = "scmPlus", errMessage = "bad argument:  " ++ (show h) }]
                        Left x -> 
                            Left $ ScmError { errCaller = "scmPlus", errMessage = "bad argument:  " ++ (show x) } : x
        Nothing -> Left $ [ScmError { errCaller = "scmPlus", errMessage = "bad argument:  " ++ (show args) }]

scmTimes :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmTimes ctx args = 
    case (cnsToList args) of
        Just x -> 
            iter x (Just 1, Just 1.0) where
                iter :: [ScmObject] -> (Maybe Int, Maybe Float) -> Either [ScmError] ScmObject
                iter [] (Just i, Just f) = Right $ ObjImmediate $ ImmInt i
                iter [] (Nothing, Just x) = Right $ ObjImmediate $ ImmFloat x
                iter (h : t) (sumi, Just f) = 
                    case (eval ctx h) of
                        Right (ObjImmediate (ImmInt x)) -> 
                            case sumi of
                                Just i -> iter t (Just $ i * x, Just $ f * (fromIntegral x))
                                otherwise -> iter t (Nothing, Just $ f * (fromIntegral x))
                        Right (ObjImmediate (ImmFloat x)) -> 
                            iter t (Nothing, Just $ f * x)
                        Right x -> 
                            Left [ScmError { errCaller = "scmTimes", errMessage = "bad argument:  " ++ (show h) }]
                        Left x -> 
                            Left $ ScmError { errCaller = "scmTimes", errMessage = "argument caused failure:  " ++ (show h) } : x
        Nothing -> Left [ScmError { errCaller = "scmTimes", errMessage = "bad argument:  " ++ (show args) }]

scmSub :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmSub ctx args = 
    case (cnsToList args) of
        Just [] -> 
            Left [ScmError { errCaller = "scmSub", errMessage = "arity must be greater than 0" }]
        Just (h : []) ->
            case (eval ctx h) of
                Right (ObjImmediate (ImmInt x)) -> Right (ObjImmediate (ImmInt $ 0 - x))
                Right (ObjImmediate (ImmFloat x)) -> Right (ObjImmediate (ImmFloat $ 0.0 - x))
                otherwise -> 
                    Left [ScmError { errCaller = "scmSub", errMessage = "bad argument:  " ++ (show h) }]
        Just x@(h : t) -> iter x (Nothing, Nothing) where
            iter :: [ScmObject] -> (Maybe Int, Maybe Float) -> Either [ScmError] ScmObject
            iter [] (Just i, Just f) = Right $ ObjImmediate $ ImmInt i
            iter [] (Nothing, Just x) = Right $ ObjImmediate $ ImmFloat x
            iter (h : t) (resi, resf) = 
                case (eval ctx h) of
                    Right (ObjImmediate (ImmInt x)) -> 
                        case (resi, resf) of
                            (Nothing, Nothing) -> iter t (Just x, Just $ fromIntegral x)
                            (Just i, Just f) -> iter t (Just $ i - x, Just $ f - (fromIntegral x))
                            (Nothing, Just f) -> iter t (Nothing, Just $ f - (fromIntegral x))
                    Right (ObjImmediate (ImmFloat x)) ->
                        case (resi, resf) of
                            (Nothing, Nothing) -> iter t (Nothing, Just x)
                            (_, Just f) -> iter t (Nothing, Just $ f - x)
                    Right x -> 
                        Left [ScmError { errCaller = "scmSub", errMessage = "bad argument:  " ++ (show h) }]
                    Left x -> 
                        Left $ ScmError { errCaller = "scmSub", errMessage = "bad argument:  " ++ (show x) } : x
        Nothing -> 
            Left [ScmError { errCaller = "scmSub", errMessage = "bad argument:  " ++ (show args) }]

scmNull :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmNull ctx args = 
    let arg = safeCar $ Just args in 
        case arg of
            Just x -> 
                case (eval ctx x) of
                    Right (ObjImmediate (ImmSym "()")) ->
                        Right symTrue
                    Right (ObjCons x) -> 
                        Right symFalse
                    Left x ->
                        Left $ ScmError { errCaller = "scmNull", errMessage = "bad argument:  " ++ (show x) } : x
                    otherwise -> 
                        Left [ScmError { errCaller = "scmNull", errMessage = "bad argument:  " ++ (show x) }]
            Nothing -> 
                Left [ScmError { errCaller = "scmNull", errMessage = "bad argument:  " ++ (show arg) }]

scmConstructor :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
scmConstructor ctx args = 
    let arg = Just args
        el = safeCar arg
        lst = safeCar $ safeCdr arg
    in 
        case (el, lst) of --should cons evaluate its argument?
            (Just e, Just l) ->
                let arg1 = eval ctx e
                    arg2 = eval ctx l
                in
                    case (arg1, arg2) of
                        (Right e, Right l) -> 
                            Right $ ObjCons ScmCons { scmCar = e, scmCdr = l }
                        (Left e, Left l) -> 
                            Left $ concat [[ScmError { errCaller = "scmConstructor", errMessage = "eval of args failed" }], e, l]
                        (Left e, Right l) -> 
                            Left $ concat [[ScmError { errCaller = "scmConstructor", errMessage = "eval of arg1 failed" }], e]
                        (Right e, Left l) -> 
                            Left $ concat [[ScmError { errCaller = "scmConstructor", errMessage = "eval of arg2 failed" }], l]                            
            otherwise -> 
                Left [ScmError { errCaller = "scmConstructor", errMessage = "arguments to cons were invalid" }]                

globalEnv :: [(String, ScmObject)] --lambda isn't a primitive; but let, let*, and letrec are
globalEnv = 
    [ ("quote", ObjPrimitive ScmPrimitive { priName = "quote", priFunction = scmQuote }) 
    , ("head", ObjPrimitive ScmPrimitive { priName = "head", priFunction = scmHead }) 
    , ("tail", ObjPrimitive ScmPrimitive { priName = "tail", priFunction = scmTail }) 
    , ("define", ObjPrimitive ScmPrimitive { priName = "define", priFunction = scmDefine })
    , ("if", ObjPrimitive ScmPrimitive { priName = "if", priFunction = scmIf })
    , ("zero?", ObjPrimitive ScmPrimitive { priName = "zero?", priFunction = scmZero })
    , ("+", ObjPrimitive ScmPrimitive { priName = "+", priFunction = scmPlus })
    , ("-", ObjPrimitive ScmPrimitive { priName = "-", priFunction = scmSub })
    , ("*", ObjPrimitive ScmPrimitive { priName = "*", priFunction = scmTimes })
    , ("null?", ObjPrimitive ScmPrimitive { priName = "null?", priFunction = scmNull })
    , ("cons", ObjPrimitive ScmPrimitive { priName = "cons", priFunction = scmConstructor })
    ]

--to do:  for blocks, if a closure is done within a let*, make a copy of it with env reversed, with tail of env (to remove items not visible)
--to do:  scmCons, +, -, *, /, if, null?, zero? etc.; deflet, defrec

{- --the environments of ghci
Prelude Data.List> tails $ reverse ["foo1", "foo2", "foo3"]
[["foo3","foo2","foo1"],["foo2","foo1"],["foo1"],[]]
Prelude Data.List> tail it
[["foo2","foo1"],["foo1"],[]]
--need to do one last reverse
-}

--to do:  define returns ctx
--filter (\ (l, v) -> l /= "foo")

findLabel :: [(String, ScmObject)] -> String -> Maybe ScmObject
findLabel env sym =
    let tgt = find (\ (lbl, value) -> lbl == sym) env in
        case tgt of
            Just (_, x) -> Just x
            Nothing -> Nothing

findLabelInBlock :: [(String, ScmObject)] -> String -> Maybe ScmBlock -> Maybe ScmObject
findLabelInBlock gbl lbl blk = iter blk where
    iter :: Maybe ScmBlock -> Maybe ScmObject
    iter Nothing = findLabel gbl lbl --find in the global env
    iter (Just ScmBlock { blkParent = p, blkBindings = b, blkType = _} ) =
        case (findLabel b lbl) of
            o@(Just _) -> o
            Nothing -> iter p --try my parent

findLabelInContext :: ScmContext -> String -> Maybe ScmObject
findLabelInContext (ScmContext { ctxStk = [], ctxEnv = e }) lbl = findLabel e lbl
findLabelInContext (ScmContext { ctxStk = (h : _), ctxEnv = e }) lbl = findLabelInBlock e lbl $ Just h

findGlobal :: String -> Maybe ScmObject --to do:  remove this
findGlobal = findLabel globalEnv

data ScmError = ScmError 
    { errCaller :: String
    , errMessage :: String }
    deriving (Eq, Show)

data ScmBlockType =
    SbtLet |
    SbtLetStar |
    SbtLetRec
    deriving (Eq, Show)

data ScmBlock = ScmBlock 
    { blkParent :: Maybe ScmBlock
    , blkType :: ScmBlockType
    , blkBindings :: [(String, ScmObject)] }
    deriving (Show)

data ScmContext = ScmContext --to do:  ctx prefix
    { ctxStk :: [ScmBlock]
    , ctxEnv :: [(String, ScmObject)] }
    deriving (Show)

--to do:  create a show-internals that will show internals of a closure (etc.)

--to do:  use fmap to make these simpler

{-
Prelude> fmap (+1) (Just 3)
Just 4
-}

--to do:  change scmCar to cnsCar and scmCdr to cnsCdr?

car :: ScmObject -> Maybe ScmObject
car (ObjCons ScmCons { scmCar = h, scmCdr = _ }) = Just h
car _ = Nothing

cdr :: ScmObject -> Maybe ScmObject
cdr (ObjCons ScmCons { scmCar = _, scmCdr = t }) = Just t
cdr _ = Nothing

safeCar :: Maybe ScmObject -> Maybe ScmObject
safeCar (Just x) = car x
safeCar Nothing = Nothing

safeCdr :: Maybe ScmObject -> Maybe ScmObject
safeCdr (Just x) = cdr x
safeCdr Nothing = Nothing

eval :: ScmContext -> ScmObject -> Either [ScmError] ScmObject
eval ctx n@(ObjImmediate _) = Right n
eval ctx (ObjSymbol x) =
    case (findLabelInContext ctx x) of 
        Just o -> eval ctx o
        Nothing -> Left [ ScmError { errCaller = "eval", errMessage = "symbol lookup failed:  " ++ x ++ ", ctx = " ++ (show ctx) } ]
eval ctx p@(ObjPrimitive _) = Right p
eval ctx (ObjCons n@(ScmCons { scmCar = ObjSymbol "lambda", scmCdr = t })) =
    let args = safeCar $ Just t
        body = safeCar $ safeCdr $ Just t
        (ScmContext { ctxEnv = e, ctxStk = s }) = ctx
    in 
        case (args, body) of
            (Just a, Just b) -> 
                Right (ObjClosure (ScmClosure { clsCtx = ctx, clsParameters = a, clsBody = b }))
            otherwise ->
                Left [ ScmError { errCaller = "eval", errMessage = "bad args or body" }] 
eval ctx (ObjCons n@(ScmCons { scmCar = h, scmCdr = t })) =
    case (eval ctx h) of
        Left l -> Left (ScmError { errCaller = "eval", errMessage = "bad function " } : l)
        Right x -> apply ctx x t
eval ctx (ObjThunk (ScmThunk { thkCtx = c, thkEvaled = e, thkValue = v })) = 
    let stk = ctxStk c --stack comes from thunk
        env = ctxEnv ctx --env comes from eval (which has the latest version of the global env)
        ectx = ScmContext { ctxStk = stk, ctxEnv = env }
    in eval ectx v
eval ctx (ObjError e) = Left [ ScmError { errCaller = "eval", errMessage = e }]
eval _ obj = Left [ ScmError { errCaller = "eval", errMessage = "could not evaluate " ++ (show obj) }]

cnsToList :: ScmObject -> Maybe [ScmObject] --lesson learned:  calling without the prime can result in a curried result (not intended!)
cnsToList x = cnsToList' x [] where
    cnsToList' :: ScmObject -> [ScmObject] -> Maybe [ScmObject]
    cnsToList' (ObjImmediate (ImmSym "()")) result = Just $ reverse result
    cnsToList' (ObjCons ScmCons { scmCar = h, scmCdr = t}) result = cnsToList' t (h : result) 
    cnsToList' _ _ = Nothing

thunkifyArgList :: ScmContext -> [ScmObject] -> [ScmObject] --thunkify anything that isn't immediate
thunkifyArgList ctx args = recur args where
    recur :: [ScmObject] -> [ScmObject]
    recur [] = []
    recur (h : t) = 
        case h of
            i@(ObjImmediate x) -> i : ( recur t)
            otherwise -> (ObjThunk ScmThunk { thkCtx = ctx, thkEvaled = False, thkValue = h }) : (recur t)

symbolsToLabels :: [ScmObject] -> [String]
symbolsToLabels lst = (iter lst []) where
    iter :: [ScmObject] -> [String] -> [String]
    iter [] res = reverse res
    iter ((ObjSymbol x) : t) res = iter t (x : res)
    iter (_ : t) res = iter t res

apply :: ScmContext -> ScmObject -> ScmObject -> Either [ScmError] ScmObject
apply ctx (ObjPrimitive ScmPrimitive { priName = nm, priFunction = fct }) args = fct ctx args
apply ctx (ObjClosure ScmClosure { clsBody = body, clsCtx = ctxOfClosure, clsParameters = params }) args =
    let arglst = cnsToList args 
        paramlst = cnsToList params
        (ScmContext { ctxStk = parent, ctxEnv = env }) = ctxOfClosure
    in
        case (arglst, paramlst) of
            (Just a, Just p) ->
                let labels = symbolsToLabels p
                    len = length a
                in 
                    if (len == length labels && len == length p) then
                        let bindings = zip labels $ thunkifyArgList ctx a --create bindings, zip params with thunkified args
                            p = if (length parent) == 0 then Nothing else Just (head parent)
                            blk = ScmBlock { blkBindings = bindings, blkParent = p, blkType = SbtLet } --create block
                        in 
                            eval (ScmContext { ctxStk = blk : parent, ctxEnv = env }) body
                    else
                        Left [ ScmError { errCaller = "apply", errMessage = "closure not implemented yet, and params <> args in length" } ]
apply ctx f args = Left [ ScmError { errCaller = "apply", errMessage = "bad function " ++ (show f) } ]

-- fac :: Int -> Int
-- fac 0 = 1
-- fac n = n * (fac $ n - 1)

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
    then (Just TokLeftParen, tail s)
    else (Nothing, s)

tokParseLeftParen :: String ->Either String (Maybe Token, String)
tokParseLeftParen s = Right $ parseLeftParen s

parseRightParen :: String -> (Maybe Token, String)
parseRightParen [] = (Nothing, [])
parseRightParen s =
    if head s == ')'
    then (Just TokRightParen, tail s)
    else (Nothing, s)

tokParseRightParen :: String -> Either String (Maybe Token, String)
tokParseRightParen s = Right $ parseRightParen s

parseSingleQuote :: String -> (Maybe Token, String)
parseSingleQuote [] = (Nothing, [])
parseSingleQuote s =
    if head s == '\''
    then (Just TokSingleQuote, tail s)
    else (Nothing, s)

tokParseSingleQuote :: String -> Either String (Maybe Token, String)
tokParseSingleQuote s = Right $ parseSingleQuote s

parseInteger :: String -> (Maybe Token, String)
parseInteger s =
    let m = s =~ "^[+|-]?[[:digit:]]+" :: String in
    if m == ""
    then (Nothing, s)
    else (Just (TokInteger m), drop (length m) s)

parseFloat :: String -> (Maybe Token, String)
parseFloat s =
    let m = s =~ "^[+|-]?[[:digit:]]+[.][[:digit:]]*" :: String in
        if m == ""
        then (Nothing, s)
        else (Just (TokFloat m), drop (length m) s)

parseDot :: String -> (Maybe Token, String)
parseDot [] = (Nothing, [])
parseDot s =
    if head s == '.'
    then (Just TokDot, tail s)
    else (Nothing, s)

parseSymbol :: String -> (Maybe Token, String)
parseSymbol [] = (Nothing, [])
parseSymbol s =
    let sym = takeWhile isSymbolChar s in
        if sym == "" 
        then (Nothing, s)
        else (Just (TokSymbol sym), drop (length sym) s)

parseAtom :: String -> (Maybe Token, String)
parseAtom [] = (Nothing, [])
parseAtom s = parseAtom' [parseDot, parseFloat, parseInteger] s

parseAtom' :: [String -> (Maybe Token, String)] -> String -> (Maybe Token, String) --to do:  refactor to equations
parseAtom' [] s = parseSymbol s
parseAtom' (h : t) s = 
    let tok = h s 
        toks = snd tok
    in
        case fst tok of
            Nothing -> parseAtom' t s
            Just x ->
                if not (null toks) && elem (head toks) symbolChars then
                    parseSymbol s --to do:  don't parse from start, just parse from beyond this point?
                else
                    tok

tokParseAtom :: String -> Either String (Maybe Token, String)
tokParseAtom s = Right $ parseAtom s

parseWhitespace :: String -> (Maybe Token, String)
parseWhitespace [] = (Nothing, [])
parseWhitespace s =
    let sym = takeWhile isWhitespace s 
    in
        if sym == "" then (Nothing, s)
        else (Just (TokWhitespace sym), drop (length sym) s)

tokParseWhitespace :: String -> Either String (Maybe Token, String)
tokParseWhitespace s = Right $ parseWhitespace s

parseComment :: String -> (Maybe Token, String)
parseComment [] = (Nothing, [])
parseComment s =
    if head s == ';' then
        let tok = takeWhile (\x -> x /= '\n') (tail s) in
        (Just (TokComment tok), drop ((length tok) + 1) s)
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

tokParseFunctions :: [String -> Either String (Maybe Token, String)]
tokParseFunctions =
    [ tokParseComment
    , tokParseLeftParen
    , tokParseRightParen
    , tokParseSingleQuote
    , tokParseWhitespace
    , tokParseAtom
    , parseString ]

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

strToTokens :: String -> Either (ScmError, String) [Token]
strToTokens [] = Right []
strToTokens s = iter tokParseFunctions s [] where
    iter :: [String -> Either String (Maybe Token, String)] -> String -> [Token] -> Either (ScmError, String) [Token]
    iter [] s tokens = Left (ScmError { errCaller = "strToTokens", errMessage = "unparsable expression:  " ++ s }, s)
    iter parsers [] tokens = Right $ reverse tokens
    iter parsers s tokens = 
        case ((head parsers) s) of
            Right (Nothing, y) -> iter (tail parsers) y tokens
            Right (Just x, y) -> iter tokParseFunctions y (x : tokens)
            Left e -> Left (ScmError { errCaller = "strToTokens", errMessage = e }, s)

{-
need test for unterminated string
need to create function for paren checking using fold and returns a tuple (current, highest, lefts, rights, errorMaybe); maybe already done a different way
need backup
-}

parseTests =
    [ ("car", [TokSymbol "car"])
    , ("(car '(a b c))", [TokLeftParen,TokSymbol "car",TokWhitespace " ",TokSingleQuote,TokLeftParen,TokSymbol "a",TokWhitespace " ",TokSymbol "b",TokWhitespace " ",TokSymbol "c",TokRightParen,TokRightParen])
    , ("(car '(a b . c))", [TokLeftParen,TokSymbol "car",TokWhitespace " ",TokSingleQuote,TokLeftParen,TokSymbol "a",TokWhitespace " ",TokSymbol "b",TokWhitespace " ",TokDot,TokWhitespace " ",TokSymbol "c",TokRightParen,TokRightParen])
    , ("(car '(234324fsdfds-sdfdsfsdf3.5))", [TokLeftParen,TokSymbol "car",TokWhitespace " ",TokSingleQuote,TokLeftParen,TokSymbol "234324fsdfds-sdfdsfsdf3.5",TokRightParen,TokRightParen])
    , ("(car (cdr (cons '(-3. . +5) (cons 4.5 23423432))))",
        [TokLeftParen
        ,TokSymbol "car"
        ,TokWhitespace " "
        ,TokLeftParen,TokSymbol "cdr"
        ,TokWhitespace " "
        ,TokLeftParen
        ,TokSymbol "cons"
        ,TokWhitespace " "
        ,TokSingleQuote
        ,TokLeftParen,TokFloat "-3."
        ,TokWhitespace " "
        ,TokDot,TokWhitespace " "
        ,TokInteger "+5"
        ,TokRightParen
        ,TokWhitespace " "
        ,TokLeftParen
        ,TokSymbol "cons"
        ,TokWhitespace " "
        ,TokFloat "4.5"
        ,TokWhitespace " "
        ,TokInteger "23423432"
        ,TokRightParen
        ,TokRightParen
        ,TokRightParen
        ,TokRightParen])
    , ( "(car '(\"a\" \"b\" \"c\"))"
      , [TokLeftParen, TokSymbol "car", TokWhitespace " ", TokSingleQuote, TokLeftParen, TokString "a", TokWhitespace " ", TokString "b", TokWhitespace " ", TokString "c", TokRightParen, TokRightParen])
    , ( "(car '(\"sdfdsfds\" 3 arsd\"sd()fdsf\"))"
      , [TokLeftParen,TokSymbol "car",TokWhitespace " ",TokSingleQuote,TokLeftParen,TokString "sdfdsfds",TokWhitespace " ",TokInteger "3",TokWhitespace " ",TokSymbol "arsd",TokString "sd()fdsf",TokRightParen,TokRightParen])
    , ( "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsfdsf()dfssdf)\" abc(sd sd d)))"
      , [TokLeftParen,TokSymbol "define",TokWhitespace " ",TokSymbol "foo",TokSingleQuote,TokLeftParen,TokSymbol "sdfsdf.sdfsdfds",TokWhitespace " ",TokSymbol "sdfds",TokString "dsfdsf",TokWhitespace " ",TokInteger "3",TokWhitespace " ",TokInteger "-3",TokWhitespace " ",TokString "fdsfdsf()dfssdf)",TokWhitespace " ",TokSymbol "abc",TokLeftParen,TokSymbol "sd",TokWhitespace " ",TokSymbol "sd",TokWhitespace " ",TokSymbol "d",TokRightParen,TokRightParen,TokRightParen])
    , ( "(define foo'(sdfsdf.sdfsdfds sdfds\"dsfdsf\" 3 -3 \"fdsf'dsf()dfssdf)\" abc(sd sd d)))"
      , [TokLeftParen,TokSymbol "define",TokWhitespace " ",TokSymbol "foo",TokSingleQuote,TokLeftParen,TokSymbol "sdfsdf.sdfsdfds",TokWhitespace " ",TokSymbol "sdfds",TokString "dsfdsf",TokWhitespace " ",TokInteger "3",TokWhitespace " ",TokInteger "-3",TokWhitespace " ",TokString "fdsf'dsf()dfssdf)",TokWhitespace " ",TokSymbol "abc",TokLeftParen,TokSymbol "sd",TokWhitespace " ",TokSymbol "sd",TokWhitespace " ",TokSymbol "d",TokRightParen,TokRightParen,TokRightParen])
    ]

-- assertBool :: String -> Bool -> Assertion
-- assertBool msg b = unless b (assertFailure msg)

-- testEmpty = TestCase $ assertEqual
--   "Should get Nothing from an empty string" Nothing ( Just "5" )

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

tokTest =
    [ TokComment "hey"
    , TokLeftParen
    , TokRightParen ]

someFunc :: IO ()
someFunc = do
    -- print $ execState test 0
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
    -- putStrLn $ show $ fac 5
    putStrLn $ show $ parseTest' parseTests
    putStrLn $ show $ getToken tokTest
    putStrLn $ show $ buildHeap [TokString "hey"]
    -- putStrLn $ show $ buildHeap [TokLeftParen, TokSymbol "a", TokSymbol "b", TokSymbol "c", TokRightParen]
    putStrLn $ show $ buildHeap [TokLeftParen, TokSymbol "a", TokLeftParen, TokSymbol "b", TokRightParen, TokSymbol "c", TokRightParen]
    -- let x = readMaybe "3" :: Integer
    -- putStrLn $ show $ readMaybe "3" :: Integer
    -- putStrLn $ show $ printHeap $ ObjAtom $ AtmSymbol "hey"
    -- putStrLn $ show $ printHeap $ buildHeap [TokLeftParen, TokSymbol "a", TokLeftParen, TokSymbol "b", TokRightParen, TokSymbol "c", TokRightParen]
    let Right (x, _) = buildHeap [TokLeftParen, TokSymbol "a", TokLeftParen, TokSymbol "b", TokRightParen, TokSymbol "c", TokRightParen] in
        putStrLn $ show $ printHeap x
    let Right (x, _) = buildHeap [TokLeftParen, TokSymbol "a", TokSymbol "b", TokSymbol "c", TokRightParen] in
        putStrLn $ show $ printHeap x
    putStrLn ("(a . c) = ")
    let Right (x, _) = buildHeap [TokLeftParen, TokSymbol "a", TokDot, TokSymbol "c", TokRightParen] in
        putStrLn $ show x
    let Right (x, _) = buildHeap [TokLeftParen, TokSymbol "a", TokDot, TokSymbol "c", TokRightParen] in
        putStrLn $ show $ printHeap x        
    putStrLn ("(a b . c) = ")
    let Right (x, _) = buildHeap [TokLeftParen, TokSymbol "a", TokSymbol "b", TokDot, TokSymbol "c", TokRightParen] in
        putStrLn $ show $ printHeap x               
    let Right (x, _) = buildHeap [TokLeftParen, TokSymbol "a", TokLeftParen, TokSymbol "b", TokSymbol "c", TokRightParen, TokSymbol "d", TokRightParen] in
        putStrLn $ show $ printHeap x
    -- putStrLn (symNil)
    let Right (x, _) = buildHeap [TokSingleQuote, TokSymbol "a"] in 
        putStrLn $ show $ printHeap x
    let Right (x, _) = buildHeap [TokFloat "3.5"] in 
        let Right y = eval (ScmContext { ctxStk = [], ctxEnv = []}) x  in 
            putStrLn $ show $ printHeap y
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