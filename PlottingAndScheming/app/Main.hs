module Main where

import Scheme
import Vector
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List

{-

to do:  add tabs for lambda calculus (using lambda symbol), SKI, X, complex, l-system, 2d, 3d
to do:  add canvas, plot a point and line (to show how this can be done) on button click
to do:  add a dropdown for selecting equations
to do:  add text box for success/failure (or radio button)
to do:  add time stamp for latest run
to do:  add text box for transcript window
to do:  research how to do a tab control
to do:  add table
to do:  add scrolling to text boxes for expression and result
to do:  parse failures should return Left (string, [Token])
to do:  possibly add untokenize button?
to do:  put all debug buttons in a debug tab

-}

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

-- foo = UI.option #+ [UI.element "1"]

cbx = UI.select #+ [string "hey"]

main :: IO ()
main = do
    -- putStrLn $ "caller is " ++ (errCaller testError)
    startGUI defaultConfig setup
 
setup :: Window -> UI ()
setup window = do
    return window # set title "Plotting and Scheming in Haskell"
    txtInput  <- UI.textarea #. "send-textarea"
    txtOutput  <- UI.textarea #. "send-textarea"
    txtScratch <- UI.textarea #. "send-textarea"
    btnClear <- UI.button #+ [string "clear result"]
    btnClearInput <- UI.button #+ [string "clear input"]
    btnTokenize <- UI.button #+ [string "tokenize"]
    btnAst <- UI.button #+ [string "create AST"]
    btnEval <- UI.button #+ [string "eval"]
    btnTest <- UI.button #+ [string "run test suite"]
    btnVecPlot <- UI.button #+ [string "plot vector"]
    divTab <- UI.div #. "header" #+ [string "plotting and scheming"]
    btnScheme <- UI.button #+ [string "scheme"] # set UI.style [("color", "blue")]
    btnVector <- UI.button #+ [string "vector"]
    btnComplex <- UI.button #+ [string "complex"]
    btn2d <- UI.button #+ [string "2d"]
    btn3d <- UI.button #+ [string "3d"]
    btnlsystem <- UI.button #+ [string "l-system"]
    btnLambda <- UI.button #+ [string "lambda"]
    btnCombinator <- UI.button #+ [string "combinator"]
    btnmrcm <- UI.button #+ [string "mrcm"]
    btnScratch <- UI.button #+ [string "scratch"]
    cbxVector <- UI.select #+ map (\(i, _) -> UI.option #+ [i]) [(string "choice1", 1), (string "choice2", 2), (string "choice3", 3)]
    canVec <- UI.canvas
    divScheme <- UI.div #+ -- #. -- "header" #+ [string "scheme"] #+
        [grid
            [ [row [element txtInput]]
            , [row [element btnClearInput, element btnTokenize, element btnAst, element btnEval, element btnTest, element btnClear]]
            , [row [element txtOutput]]
            ]
        ]
    divVector <- UI.div #+ -- #. -- "header" #+ [string "vector"] #+
        [grid
            [ [row [element cbxVector]]
            , [row [element canVec]]
            , [row [element btnVecPlot]] 
            ]
        ]
    divScratch <- UI.div #+
        [grid [[row [element txtScratch]]]]
    -- elResult <- UI.span --to do:  remove this?

    getBody window #+ 
        [ UI.div #+ 
            [ element btnScheme, element btnVector, element btnComplex, element btnlsystem, element btnmrcm 
            , element btn2d, element btn3d, element btnLambda, element btnCombinator, element btnScratch]
        , element divScheme 
        , element divVector
            # set UI.style [("display", "none")]
        ]

    on UI.click btnScheme $ const $ do
        element divScheme 
            # set UI.style [("display", "block")] 
        element divVector
            # set UI.style [("display", "none")]
        element btnScheme
            # set UI.style [("color", "blue")]
        element btnVector
            # set UI.style [("color", "black")]
    
    on UI.click btnVector $ const $ do
        element divScheme 
            # set UI.style [("display", "none")]
        element divVector
            # set UI.style [("display", "block")]
        element btnScheme
            # set UI.style [("color", "black")]
        element btnVector
            # set UI.style [("color", "blue")]            

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

    on UI.click btnVecPlot $ const $ do
        canVec # drawLines [((0,0), (20, 20)), ((20, 20), (80, 20)), ((80, 20), (50, 50)) ]
        -- canVec # line (0, 0) (20, 20)
        canVec # UI.fillRect (50, 50) 1 1 --set pixel; to do:  package this up as function
        return canVec