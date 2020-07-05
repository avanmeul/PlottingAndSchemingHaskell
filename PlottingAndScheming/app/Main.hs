module Main where

import Lib
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    -- putStrLn $ show (tokParse "3")
    startGUI defaultConfig setup

canvasSize = 400

strToTok :: String -> String
strToTok s =
    let res = tokParse s
    in
        case res of 
            Left x -> "error:  " ++ x
            Right x -> "success:  " ++ (show x)

strToHeap :: String -> Either (String, [Token]) (ScmObject, [Token])
strToHeap x = 
    let toks = tokParse x 
    in 
        case toks of
            Right x -> buildHeap $ toksNoWhitespace x 
            Left x -> Left (x, [])

strToAst :: String -> Either String String
strToAst x = 
    let hp = strToHeap x
    in
        case hp of
            Right (x, []) -> Right $ printHeap x
            Right (x, t) -> Left "unconsumed tokens"
            Left (x, _) -> Left x

strToEval :: String -> Either [ScmError] ScmObject
strToEval x = 
    let hp = strToHeap x
    in
        case hp of
            Right (e, []) -> 
                eval (ScmContext { ctxStk = [], ctxEnv = globalEnv }) e 
            Right (x, t) -> Left [ ScmError { errCaller = "strToEval", errMessage = "unconsumed tokens" } ]
            Left (x, _) -> Left [ ScmError { errCaller = "strToEval", errMessage = x } ]

strToEvalStr :: String -> String
strToEvalStr x =
    case (strToEval x) of
        Right x -> "success:  " ++ (printHeap x)
        Left x -> "failure:  " ++ (show x)

setup :: Window -> UI ()
setup window = do
    return window # set title "Plotting and Scheming in Haskell"

    btnClear <- UI.button #+ [string "clear result"]
    btnTokenize <- UI.button #+ [string "tokenize"]
    btnAst <- UI.button #+ [string "create AST"]
    btnEval <- UI.button #+ [string "eval"]
    btnTest <- UI.button #+ [string "run test suite"]
    --to do:  add button for clearing result
    --to do:  add text box for success/failure (or radio button)
    --to do:  add time stamp for latest run
    --to do:  add text box for transcript window
    --to do:  research how to do a tab control
    --to do:  add table
    --to do:  add scrolling to text boxes for expression and result
    --to do:  parse failures should return Left (string, [Token])
    --to do:  possibly add untokenize button?
    --to do:  put all debug buttons in a debug tab
    --to do:  get rid of all self-evaluating symbols out of the symbol table
    --to do:  Left [scmError] for errors where scmError is a record with caller name and error description

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

    on UI.click btnAst $ const $ do
        expression <- get value inputExpression
        element elResult # set UI.text (show $ strToAst expression)

    on UI.click btnEval $ const $ do
        expression <- get value inputExpression
        element elResult # set UI.text (strToEvalStr expression)

    on UI.click btnClear $ const $ do
        element elResult # set UI.text ""    