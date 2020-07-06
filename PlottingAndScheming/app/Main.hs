module Main where

import Lib
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

-- testError = ScmError { errCaller = "fido", errMessage = "hey you" }

main :: IO ()
main = do
    -- putStrLn $ "caller is " ++ (errCaller testError)
    -- expression <- get value inputExpression
    -- putStrLn $ show (tokParse "3")
    -- putStrLn $ show (filter ((\ x (l, v) -> l /= x) "foo") junk)
    -- putStrLn $ show $ strToHeaps expression
    startGUI defaultConfig setup

strToTok :: String -> String
strToTok s =
    let res = tokParse s
    in
        case res of
            Left x -> "error:  " ++ x
            Right x -> "success:  " ++ (show x)

strToHeaps :: String -> Either ([ScmError], [Token]) [ScmObject]
strToHeaps x =
    case (strToTokens x) of
        Right x -> iter [] (toksNoWhitespace x) where
            iter :: [ScmObject] -> [Token] -> Either ([ScmError], [Token]) [ScmObject]
            iter res [] = Right $ reverse res
            iter res toks =
                case (buildHeap toks) of
                    Right (o, t) -> iter (o : res) t
                    Left (s, t) -> Left ( [ScmError { errCaller = "strToHeaps", errMessage = s}], t)
        Left (e, t) -> Left ((ScmError { errCaller = "strToHeaps", errMessage = "tokenization failed, can't build heaps" } : [e]), [])

evalHeaps :: ScmContext -> [ScmObject] -> Either [ScmError] [(ScmObject, ScmObject)] --the tuple is (before, after) evaluations
evalHeaps ctx lst = iter ctx lst [] where
    iter :: ScmContext -> [ScmObject] -> [(ScmObject, ScmObject)] -> Either [ScmError] [(ScmObject, ScmObject)]
    iter ctx [] res = Right $ reverse res
    iter ctx (h : t) res = 
        case (eval ctx h) of 
            Right x@(ObjContext c) -> iter c t $ (h, x) : res
            Right x -> iter ctx t $ (h, x) : res
            Left x -> Left (ScmError { errCaller = "evalHeaps", errMessage = "failed in evaluating " ++ (show h) } : x)

evalResults :: String -> String
evalResults str =
    case (strToHeaps str) of
        Right x -> 
            case (evalHeaps (ScmContext { ctxStk = [], ctxEnv = globalEnv }) x) of
                Right x -> 
                    concat $ intersperse "\r\n" $ fmap (\ (_, r) -> printHeap r) x
                Left x -> show x
        Left x -> show x

heapifyResults :: String -> String
heapifyResults str =
    case (strToHeaps str) of
        Right x -> concat $ intersperse "\r\n" $ fmap printHeap x
        Left x -> show x        

setup :: Window -> UI ()
setup window = do
    return window # set title "Plotting and Scheming in Haskell"

    btnClear <- UI.button #+ [string "clear result"]
    btnTokenize <- UI.button #+ [string "tokenize"]
    btnAst <- UI.button #+ [string "create AST"]
    btnEval <- UI.button #+ [string "eval"]
    btnTest <- UI.button #+ [string "run test suite"]
    --to do:  add text box for success/failure (or radio button)
    --to do:  add time stamp for latest run
    --to do:  add text box for transcript window
    --to do:  research how to do a tab control
    --to do:  add table
    --to do:  add scrolling to text boxes for expression and result
    --to do:  parse failures should return Left (string, [Token])
    --to do:  possibly add untokenize button?
    --to do:  put all debug buttons in a debug tab

    --to do:  change this from input box into text box?
    inputExpression <- UI.input --to do:  change the name of this
        # set (attr "placeholder") "expression to be evaluated"

    elResult <- UI.span

    getBody window #+
        [ element inputExpression
        , element btnTokenize
        , element btnAst
        , element btnEval
        , element btnTest
        , element btnClear
        , element elResult
        ]

    on UI.click btnTokenize $ const $ do
        expression <- get value inputExpression
        element elResult # set UI.text (strToTok expression)

    on UI.click btnTest $ const $ do
        element elResult # set UI.text (show $ parseTest' parseTests)

    on UI.click btnAst $ const $ do --to do:  make this work for expressions
        expression <- get value inputExpression
        element elResult # set UI.text (heapifyResults expression)

    on UI.click btnEval $ const $ do
        expression <- get value inputExpression
        element elResult # set UI.text (evalResults expression)

    on UI.click btnClear $ const $ do
        element elResult # set UI.text ""