module Main where

import Scheme
import Vector
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List

{-

to do:  lambda calculus should use lambda symbol
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

toggleTabs :: [(UI.Element, UI.Element)] -> UI ()
toggleTabs [] = return ()
toggleTabs ((d, b) : t) = do
    element d
        # set UI.style [("display", "none")]
    element b
        # set UI.style [("color", "black")]
    toggleTabs t

toggleTab :: UI.Element -> UI.Element -> UI ()
toggleTab d b = do
    element d
        # set UI.style [("display", "block")]
    element b
        # set UI.style [("color", "blue")]
    return ()

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

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
    btnLsystem <- UI.button #+ [string "l-system"]
    btnLambda <- UI.button #+ [string "lambda"]
    btnCombinator <- UI.button #+ [string "combinator"]
    btnMrcm <- UI.button #+ [string "mrcm"]
    btnScratch <- UI.button #+ [string "scratch"]
    -- cbxVector <- UI.select #+ [UI.option #+ [string "1"], UI.option #+ [string "2"]] 
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
    divComplex <- UI.div
    div2d <- UI.div
    div3d <- UI.div
    divLsystem <- UI.div
    divMrcm <- UI.div
    divLambda <- UI.div
    divCombinator <- UI.div
    divScratch <- UI.div #+
        [grid [[row [element txtScratch]]]]

    getBody window #+ 
        [ UI.div #+ 
            [ element btnScheme, element btnVector, element btnComplex, element btnLsystem
            , element btn2d, element btn3d, element btnMrcm, element btnLambda, element btnCombinator, element btnScratch]
        , element divScheme 
        , element divVector
            # set UI.style [("display", "none")]
        , element divComplex
            # set UI.style [("display", "none")]
        , element divLsystem
            # set UI.style [("display", "none")]  
        , element div2d
            # set UI.style [("display", "none")]
        , element div3d
            # set UI.style [("display", "none")] 
        , element divLambda
            # set UI.style [("display", "none")]
        , element divCombinator
            # set UI.style [("display", "none")]    
        , element divScratch
            # set UI.style [("display", "none")]                                                           
        ]

    let tabs :: [(UI.Element, UI.Element)]
        tabs = 
            [ (divScheme, btnScheme)
            , (divVector, btnVector)
            , (div2d, btn2d)
            , (div3d, btn3d)
            , (divLsystem, btnLsystem)
            , (divLambda, btnLambda)
            , (divCombinator, btnCombinator)
            , (divScratch, btnScratch)
            , (divComplex, btnComplex)
            , (divMrcm, btnMrcm) ]

    on UI.click btnScheme $ const $ do
        toggleTabs tabs
        toggleTab divScheme btnScheme
    
    on UI.click btnVector $ const $ do
        toggleTabs tabs
        toggleTab divVector btnVector          

    on UI.click btn2d $ const $ do
        toggleTabs tabs
        toggleTab div2d btn2d

    on UI.click btn3d $ const $ do
        toggleTabs tabs
        toggleTab div3d btn3d

    on UI.click btnLsystem $ const $ do
        toggleTabs tabs
        toggleTab divLsystem btnLsystem
        
    on UI.click btnLambda $ const $ do
        toggleTabs tabs
        toggleTab divLambda btnLambda

    on UI.click btnCombinator $ const $ do
        toggleTabs tabs
        toggleTab divCombinator btnCombinator

    on UI.click btnScratch $ const $ do
        toggleTabs tabs
        toggleTab divScratch btnScratch

    on UI.click btnComplex $ const $ do
        toggleTabs tabs
        toggleTab divComplex btnComplex   
        
    on UI.click btnMrcm $ const $ do
        toggleTabs tabs
        toggleTab divMrcm btnMrcm          

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