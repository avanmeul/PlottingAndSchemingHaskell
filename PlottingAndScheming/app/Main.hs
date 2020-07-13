module Main where

import Lib
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

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
    -- putStrLn $ "caller is " ++ (errCaller testError)
    startGUI defaultConfig setup

-- draw a single line
line :: UI.Point -> UI.Point -> Element -> UI ()
line xy1 xy2 c = do
    c # UI.beginPath
    c # UI.moveTo xy1
    c # UI.lineTo xy2
    c # UI.closePath
    c # UI.stroke
   
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
    btnVecPlot <- UI.button #+ [string "plot vector"]
    divTab <- UI.div #. "header" #+ [string "plotting and scheming"]
    btnScheme <- UI.button #+ [string "scheme"] # set UI.style [("color", "blue")]
    btnVector <- UI.button #+ [string "vector"]
    btnComplex <- UI.button #+ [string "complex"]
    btn2d <- UI.button #+ [string "2d"]
    btn3d <- UI.button #+ [string "3d"]
    btnlsystem <- UI.button #+ [string "l-system"]
    btnLambda <- UI.button #+ [string "lambda"]
    btnski <- UI.button #+ [string "ski"]
    btnx <- UI.button #+ [string "x"]
    btnmrcm <- UI.button #+ [string "mrcm"]
    btnScratch <- UI.button #+ [string "scratch"]
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
            [ [row [element canVec]]
            , [row [element btnVecPlot]]]
        ]
    elResult <- UI.span --to do:  remove this?

    getBody window #+ 
        [ UI.div #+ 
            [ element btnScheme, element btnVector, element btnComplex, element btnlsystem, element btnmrcm 
            , element btn2d, element btn3d, element btnLambda, element btnski, element btnx, element btnScratch]
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
        canVec # line (0, 0) (20, 20)
        canVec # UI.fillRect (50, 50) 1 1 --set pixel; to do:  package this up as function
        return canVec