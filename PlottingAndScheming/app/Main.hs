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

setup :: Window -> UI ()
setup window = do
    return window # set title "Plotting and Scheming in Haskell"

    clear     <- UI.button #+ [string "Clear the canvas."]
    myButton  <- UI.button #+ [string "eval"]

    --to do:  change this from input box into text box?
    userNameInput <- UI.input
        # set (attr "placeholder") "expression to be evaluated"

    elResult <- UI.span

    getBody window #+
        [ element userNameInput
        , element myButton, element elResult
        ]

    on UI.click myButton $ const $ do
        userName <- get value userNameInput
        -- let foo = tokParse userName
        -- in
        --     let res :: String
        --         res =
        --         case foo of
        --             Left x -> x
        --             Right x -> show x 
        --     in 
        element elResult # set UI.text (strToTok userName)
