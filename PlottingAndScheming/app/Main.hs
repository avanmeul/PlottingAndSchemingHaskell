module Main where

import Lib

-- main :: IO ()
-- main = do
--     putStrLn "hery"
--     -- someFunc

import Control.Monad
-- import Paths
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    -- static <- getStaticDir
    startGUI defaultConfig setup

canvasSize = 400

strToTok :: String -> String
strToTok s =
    let res = tokParse s
    in
        case res of 
            Left x -> "error:  " ++ x
            Right x -> "success:  " ++ (show x)

strToAst :: String -> Either String String
strToAst x = 
    let toks = tokParse x 
    in 
        case toks of
            Right x -> 
                let ast = buildHeap $ toksNoWhitespace x in 
                    case ast of
                        Right (x, []) -> Right $ printHeap x
                        Right (x, t) -> Left "unconsumed tokens"
                        Left (x, _) -> Left x
            Left x -> Left x

setup :: Window -> UI ()
setup window = do
    return window # set title "Plotting and Scheming in Haskell"

    clear     <- UI.button #+ [string "Clear the canvas."]
    btnTokenize  <- UI.button #+ [string "tokenize"]
    btnAst  <- UI.button #+ [string "create AST"]
    btnEval  <- UI.button #+ [string "eval"]
    btnTest  <- UI.button #+ [string "run test suite"]
    --to do:  add text box for success/failure (or radio button)
    --to do:  add time stamp for latest run
    --to do:  add text box for transcript window
    --to do:  research how to do a tab control
    --to do:  add table
    --to do:  add scrolling to text boxes for expression and result

    --to do:  change this from input box into text box?
    userNameInput <- UI.input --to do:  change the name of this
        # set (attr "placeholder") "expression to be evaluated"

    elResult <- UI.span

    getBody window #+
        [ element userNameInput
        , element btnTokenize
        , element btnAst
        , element btnEval
        , element btnTest
        , element elResult
        ]

    on UI.click btnTokenize $ const $ do
        userName <- get value userNameInput
        element elResult # set UI.text (strToTok userName)

    on UI.click btnTest $ const $ do
        element elResult # set UI.text (show $ parseTest' parseTests)

    on UI.click btnAst $ const $ do
        userName <- get value userNameInput
        element elResult # set UI.text (show $ strToAst userName)

    on UI.click btnEval $ const $ do
        element elResult # set UI.text "not implemented yet"