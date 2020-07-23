module Main where

{-# LANGUAGE OverloadedStrings #-}

import Scheme
import Vector
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List

{-

to do:  package up all xml
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

--to do:  use explicit recursion to go through the tabs only once, setting the one of interest, and resetting the ones not of interest

toggleTabs :: [(UI.Element, UI.Element)] -> UI ()
toggleTabs [] = return ()
toggleTabs ((d, b) : t) = do
    UI.element d
        # set UI.style [("display", "none")]
    UI.element b
        # set UI.style [("color", "black")]
    toggleTabs t

toggleTab :: UI.Element -> UI.Element -> UI ()
toggleTab d b = do
    UI.element d
        # set UI.style [("display", "block")]
    UI.element b
        # set UI.style [("color", "blue")]
    return ()

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

canvasSize = 400

main :: IO ()
main = do
    names <- getXmlVector "PlottingAndScheming/xml/vector.xml"
    startGUI defaultConfig $ setup names -- $ map T.unpack names
 
setup :: [String] -> Window -> UI ()
setup plots window = do
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
    -- cbxVector <- UI.select #+ map (\i -> UI.option #+ [i]) ["one", "two", "three"]
    -- let plots = do
        -- getVectorPlots
    cbxVector <- UI.select #+ map (\i -> UI.option #+ [string i]) plots --["one", "two", "three"]
    -- cbxVector <- UI.select #+ map (\(i, _) -> UI.option #+ [i]) [(string "choice1", 1), (string "choice2", 2), (string "choice3", 3)]
    -- cbxVector <- UI.select #+ map (\i -> UI.option #+ [i]) plots --[(string "choice1", 1), (string "choice2", 2), (string "choice3", 3)]
    canVec <- UI.canvas
        # set UI.height canvasSize
        # set UI.width  canvasSize
    divScheme <- UI.div #+ -- #. -- "header" #+ [string "scheme"] #+
        [grid
            [ [row [UI.element txtInput]]
            , [row [UI.element btnClearInput, UI.element btnTokenize, UI.element btnAst, UI.element btnEval, UI.element btnTest, UI.element btnClear]]
            , [row [UI.element txtOutput]]
            ]
        ]
    divVector <- UI.div #+ -- #. -- "header" #+ [string "vector"] #+
        [grid
            [ [row [UI.element cbxVector]]
            , [row [UI.element canVec]]
            , [row [UI.element btnVecPlot]] 
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
        [grid [[row [UI.element txtScratch]]]]

    getBody window #+ 
        [ UI.div #+ 
            [ UI.element btnScheme, UI.element btnVector, UI.element btnComplex, UI.element btnLsystem
            , UI.element btn2d, UI.element btn3d, UI.element btnMrcm, UI.element btnLambda, UI.element btnCombinator, UI.element btnScratch]
        , UI.element divScheme 
        , UI.element divVector
            # set UI.style [("display", "none")]
        , UI.element divComplex
            # set UI.style [("display", "none")]
        , UI.element divLsystem
            # set UI.style [("display", "none")]  
        , UI.element div2d
            # set UI.style [("display", "none")]
        , UI.element div3d
            # set UI.style [("display", "none")] 
        , UI.element divLambda
            # set UI.style [("display", "none")]
        , UI.element divCombinator
            # set UI.style [("display", "none")]    
        , UI.element divScratch
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
        UI.element txtOutput # set UI.text (strToTok expression)

    on UI.click btnTest $ const $ do
        UI.element txtOutput # set UI.text (show $ parseTest' parseTests)

    on UI.click btnAst $ const $ do
        expression <- get value txtInput
        UI.element txtOutput # set UI.text (heapifyResults expression)

    on UI.click btnEval $ const $ do
        expression <- get value txtInput
        UI.element txtOutput # set UI.text (evalResults expression)

    on UI.click btnClear $ const $ do
        UI.element txtOutput # set UI.text ""

    on UI.click btnClearInput $ const $ do
        UI.element txtInput # set value ""

    on UI.click btnVecPlot $ const $ do
        canVec # drawLines [((0,0), (20, 20)), ((20, 20), (80, 20)), ((80, 20), (50, 50)) ]
        -- canVec # line (0, 0) (20, 20)
        -- canVec # UI.fillRect (50, 50) 1 1 --set pixel; to do:  package this up as function
        canVec # setPixel (50, 50) "grey"
        canVec # setPixel (51, 51) "grey"
        canVec # setPixel (52, 52) "grey"
        return canVec