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
        Right x -> iter [] (toksRemoveNonSemantic x) where
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

main :: IO ()
main = do
    -- putStrLn $ "caller is " ++ (errCaller testError)
    startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Plotting and Scheming in Haskell"
    txtInput  <- UI.textarea #. "send-textarea"
    txtOutput  <- UI.textarea #. "send-textarea"
    btnClear <- UI.button #+ [string "clear result"]
    btnClearInput <- UI.button #+ [string "clear input"]
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

    elResult <- UI.span

    getBody window #+ [grid
        [[row [element txtInput]]
        ,[row [element btnClearInput, element btnTokenize, element btnAst, element btnEval, element btnTest, element btnClear]]
        ,[row [element txtOutput]]]
        ]

    on UI.click btnTokenize $ const $ do
        expression <- get value txtInput
        element txtOutput # set UI.text (strToTok expression)

    on UI.click btnTest $ const $ do
        element txtOutput # set UI.text (show $ parseTest' parseTests)

    on UI.click btnAst $ const $ do
        expression <- get value txtInput
        element txtOutput # set UI.text (heapifyResults expression)

    on UI.click btnEval $ const $ do
        expression <- get value txtInput
        element txtOutput # set UI.text (evalResults expression)

    on UI.click btnClear $ const $ do
        element txtOutput # set UI.text ""

    on UI.click btnClearInput $ const $ do
        element txtInput # set value ""