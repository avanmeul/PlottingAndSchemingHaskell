module Main where

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import Scheme
import Vector
import Complex
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List

{-

to do:  

fix plots that are broken:  
    bee hive (0 vectors),
    sierpinski's gasket #2 (0 vectors), 
    tiles (0 vectors)
    tiles 2 (0 vectors)
allow drawing seed (generation 0)
define pi in scheme
bug using car/cdr (in certain contexts)
coloring for monkeys tree and mandelbrot doesn't match F# version
package up all xml
lambda calculus should use lambda symbol
add text box for success/failure (or radio button)
add time stamp for latest run
add text box for transcript window
research how to do a tab control
add table
add scrolling to text boxes for expression and result
parse failures should return Left (string, [Token])
possibly add untokenize button?
put all debug buttons in a debug tab

-}

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

-- fetchRule :: Maybe XmlObj -> String
-- fetchRule plotObj =
--     case plotObj of
--         Just x -> xobRules x
--         otherwise -> ""

debugPlot :: [XmlObj] -> Int -> Either [ScmError] [Vector]
debugPlot plots i =
    let plotObj = fetchVectorXmlObj plots $ Just i
    in case plotObj of
        Just x -> vectorFractal x
        Nothing -> Left [ScmError { errMessage = "barf", errCaller = "debug "}]

getVectors :: [XmlObj] -> Maybe Int -> (([Vector], (Maybe Int, Maybe Int)), String)
getVectors plots i = 
    let plotObj = fetchVectorXmlObj plots i
    in 
        case plotObj of
            Just x -> 
                case (vectorFractal x) of
                    Right x -> 
                        let v@(vecs', _) = normalizeVectors x
                        in (v, "number of vectors:  " ++ (show $ length vecs'))
                    Left x -> 
                        (([], (Just 100, Just 100)), "failed to get xml for plot, error:  " ++ (show x))
            otherwise -> 
               (([], (Just 100, Just 100)), "failed to get vectors for plot")

printHello :: IO ()
printHello = do
    putStrLn "hello"

main :: IO ()
main = do
    -- x <- printHello
    -- _ <- x
    -- return ()

    names <- parseXmlVector "PlottingAndScheming/xml/vector.xml"
    complexPlots <- parseXmlComplex "PlottingAndScheming/xml/complex.xml"
    
    -- index <- Just 8 --get UI.selection cbxVector
    -- let plotObj = fetchVectorXmlObj names $ Just 8
    --     vecs = 
    --         case plotObj of
    --             Just x -> vectorFractal x
    --             otherwise -> Left [ScmError { errMessage = "could not get plot object", errCaller = "main" }]
    -- putStrLn $ show vecs
    -- let obj = head names --plotObj = fetchVectorXmlObj names 0 -- index
    -- vecs = vectorFractal obj
    -- putStrLn "going to call vector fractal"
    -- putStrLn $ "vecs = " ++ (show $ vectorFractal $ head names)
    -- let plotObj = fetchVectorPlot names $ Just 1
    -- let plotObj = fetchVectorXmlObj names $ Just 1
    --     rules = fetchRule plotObj
    --     evaled = evalString rules
    --     evaledStr = 
    --         case evaled of
    --             Right x -> printHeap $ last x
    --             Left x -> show x
    --     -- evaled' = evalHeaps rules
    -- putStrLn $ "evaled = " ++ evaledStr    
    -- putStrLn $ show $ names !! 11
    -- let vecs = debugPlot names 8 --8 is bee hive built-in
    -- putStrLn $ show vecs
    startGUI defaultConfig $ setup (names, complexPlots)

canvasSize :: Int
canvasSize = 50

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

fetchVectorXmlObj :: [XmlObj] -> Maybe Int -> Maybe XmlObj
fetchVectorXmlObj plots i = 
    case i of
        Just x -> Just $ plots !! x
        otherwise -> Nothing

fetchVectorPlot :: [XmlObj] -> Maybe Int -> String
fetchVectorPlot plots i = maybe "" show $ fetchVectorXmlObj plots i

fetchVectorXmlComplex :: [XmlComplex] -> Maybe Int -> Maybe XmlComplex
fetchVectorXmlComplex plots i = 
    case i of
        Just x -> Just $ plots !! x
        otherwise -> Nothing

fetchComplexPlot :: [XmlComplex] -> Maybe Int -> String
fetchComplexPlot plots i = maybe "" show $ fetchVectorXmlComplex plots i
 
setup :: ([XmlObj], [XmlComplex]) -> Window -> UI ()
setup (xmlVec, xmlComplex) window = do
    return window # set title "Plotting and Scheming in Haskell"
    txtInput  <- UI.textarea #. "send-textarea"
    txtOutput  <- UI.textarea #. "send-textarea"
    txtScratch <- UI.textarea #. "send-textarea"
    txtVector <- UI.textarea #. "send-textarea"
    txtVectorTranscript <- UI.textarea #. "send-textarea"
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
    cbxVector <- UI.select #+ map (\XmlObj { xobName = i } -> UI.option #+ [string i]) xmlVec
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
            , [row [UI.element txtVector]]
            , [row [UI.element btnVecPlot]] 
            , [row [UI.element canVec]]
            , [row [UI.element txtVectorTranscript]]
            ]
        ]
    --complex
    cbxComplex <-  UI.select #+ map (\XmlComplex { xcmName = i } -> UI.option #+ [string i]) xmlComplex
    txtComplex <- UI.textarea #. "send-textarea"
    btnComplexPlot <- UI.button #+ [string "plot complex"]
    canComplex <- UI.canvas
        # set UI.height canvasSize
        # set UI.width  canvasSize  
    txtComplexTranscript <- UI.textarea #. "send-textarea"          
    divComplex <- UI.div #+
        [grid 
            [ [row [UI.element cbxComplex]]
            , [row [UI.element txtComplex]]
            , [row [UI.element btnComplexPlot]]
            , [row [UI.element canComplex]]
            , [row [UI.element txtComplexTranscript]]
            ]
        ]
    --2d
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

    on UI.click cbxVector $ const $ do
        index <- get UI.selection cbxVector
        let plotObj = fetchVectorPlot xmlVec index
        UI.element txtVector # set UI.text (show plotObj)
        UI.element txtVectorTranscript # set UI.text ("index:  " ++ (show $ maybe (-1) id index))

    on UI.click btnVecPlot $ const $ do
        index <- get UI.selection cbxVector
        let ((vecs, (width, height)), msg) = getVectors xmlVec index
        UI.element canVec # set UI.width (maybe 200 id width)
        UI.element canVec # set UI.height (maybe 200 id height)
        canVec # drawVecs vecs
        UI.element txtVectorTranscript # set UI.text msg
        return canVec        

    on UI.click cbxComplex $ const $ do
        index <- get UI.selection cbxComplex
        let plotObj = fetchComplexPlot xmlComplex index
        UI.element txtComplex # set UI.text (show plotObj)
        UI.element txtComplexTranscript # set UI.text "under construction"
    
    on UI.click btnComplexPlot $ const $ do
        UI.element txtComplexTranscript # set UI.text "hey"