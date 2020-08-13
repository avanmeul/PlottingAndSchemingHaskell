module Vector where

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Scheme
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Text.XML
import Prelude hiding (readFile, writeFile)
import Text.XML as X
import Text.XML.Cursor as C
import qualified Data.Text as T
import Text.Read
import Data.List
import Data.Maybe

{-
Copyright (c) 2020, 2015, 2009, 2008, 2007, 2007 by André Marc van Meulebrouck.  All rights reserved worldwide.
-}

--to do:  line record:  thickness = 2, color, p1, p2

data Vector = Vector 
    { vecP1 :: UI.Point
    , vecP2 :: UI.Point
    , vecColor :: Color }
    deriving (Show)

type Color = String 

setPixel :: UI.Point -> Color -> UI.Element -> UI () --to do:  pass color as UI.Color, make first arg
setPixel pt clr can = do
    -- c # set' UI.strokeStyle "yellow"
    can # set' UI.fillStyle (UI.htmlColor clr)
    can # UI.fillRect pt 1 1

drawVec :: Vector -> UI.Element -> UI ()
drawVec (Vector { vecP1 = p1, vecP2 = p2, vecColor = clr }) c = do
    c # set' UI.strokeStyle clr
    c # UI.beginPath
    c # UI.moveTo p1
    c # UI.lineTo p2
    c # UI.closePath
    c # UI.stroke

drawVecs :: [Vector] -> UI.Element -> UI ()
drawVecs vecs c = iter vecs where
    iter :: [Vector] -> UI ()
    iter [] = return ()
    iter (h : t) = do
        drawVec h c
        iter t

data Extremes = Extremes
    { xLo :: Maybe Double
    , xHi :: Maybe Double
    , yLo :: Maybe Double
    , yHi :: Maybe Double }
    deriving (Show)

--to do:  use x,y from point 1 for initial values of extrema

checkCoordinate :: Double -> (Maybe Double, Maybe Double) -> (Maybe Double, Maybe Double)
checkCoordinate = undefined

checkPair :: Double -> Double -> (Maybe Double, Maybe Double) -> (Maybe Double, Maybe Double)
checkPair arg1 arg2 (Just lo, Just hi) =
    let sorted = sort [arg1, arg2, lo, hi]
    in (Just $ head sorted, Just $ last sorted)
checkPair arg1 arg2 (Nothing, Nothing) = 
    if arg1 < arg2 then (Just arg1, Just arg2) else (Just arg2, Just arg1)
checkPair arg1 arg2 (Just lo, Nothing) = 
    let sorted = sort [arg1, arg2, lo]
    in (Just $ head sorted, Just $ last sorted)
checkPair arg1 arg2 (Nothing, Just hi) =
    let sorted = sort [arg1, arg2, hi]
    in (Just $ head sorted, Just $ last sorted)

checkExtrema :: Vector -> Extremes -> Extremes
checkExtrema (Vector { vecP1 = (x1, y1), vecP2 = (x2, y2) }) (Extremes { xLo = xLow, xHi = xHigh, yLo = yLow, yHi = yHigh }) = 
    let (xlo, xhi) = checkPair x1 x2 (xLow, xHigh)
        (ylo, yhi) = checkPair y1 y2 (yLow, yHigh)
    in Extremes { xLo = xlo, xHi = xhi, yLo = ylo, yHi = yhi }

findExtrema :: [Vector] -> Extremes
findExtrema vecs = 
    foldr checkExtrema (Extremes { xLo = Nothing, xHi = Nothing, yLo = Nothing, yHi = Nothing }) vecs
    
normalizeVectors :: [Vector] -> ([Vector], (Maybe Int, Maybe Int))
normalizeVectors vecs =
    case (findExtrema vecs) of
        (Extremes { xLo = Just xl, xHi = Just xh, yLo = Just yl, yHi = Just yh }) ->
            let width = xh - xl
                height = yh - yl
                adjust = \(Vector { vecP1 = (x1, y1), vecP2 = (x2,y2), vecColor = c }) -> 
                    let x1' = x1 - xl
                        -- y1' = yh - y1
                        y1' = y1 - yl
                        x2' = x2 - xl
                        y2' = y2 - yl
                        -- y2' = yh - y2
                    in Vector { vecP1 = (x1', y1'), vecP2 = (x2', y2'), vecColor = c }
                vecs' = map adjust vecs
            in (vecs', (Just $ ceiling width, Just $ ceiling height))
        otherwise -> 
            (vecs, (Nothing, Nothing))

--to do:  level should have Level and Image should have Subtractor via a newtype

data ColoringAlgorithm =
    CalLevel Int | --Level
    CalImage Int --Subtractor
    deriving (Eq, Show)

data XmlObj = XmlObj
    { xobName :: String
    , xobDesc :: String
    , xobGenerations :: Int
    , xobLength :: Double
    , xobRules :: String
    , xobBuiltIn :: Bool
    , xobContinuous :: Bool --to do
    , xobColors :: [Color]
    , xobAlgorithm :: ColoringAlgorithm
    } deriving (Eq, Show)

ctorColoringAlgorithm :: String -> [Cursor] -> Maybe ColoringAlgorithm
ctorColoringAlgorithm typ el =
    case typ of
        "level" -> 
            let nm = Name { nameLocalName = T.pack "level", nameNamespace = Nothing, namePrefix = Nothing }
                lvlActual = findLevel nm
            in
                Just $ CalLevel lvlActual
        "image" -> 
            let nm = Name { nameLocalName = T.pack "subtractor", nameNamespace = Nothing, namePrefix = Nothing }
                lvlActual = findLevel nm
            in
                Just $ CalImage  lvlActual
        otherwise -> 
            Nothing
    where 
        findLevel :: Name -> Int
        findLevel nm =
            let lvl = el >>= C.element nm >>= child >>= content
                lvlUnpack = map T.unpack lvl
                lvlMaybe = if null lvlUnpack then Nothing else readMaybe (head lvlUnpack) :: Maybe Int
            in maybe 1 id lvlMaybe     

ctorXmlObj :: [Cursor] -> XmlObj
ctorXmlObj el =
    let 
        info = Name { nameLocalName = T.pack "info", nameNamespace = Nothing, namePrefix = Nothing }
        nm = Name { nameLocalName = T.pack "name", nameNamespace = Nothing, namePrefix = Nothing }
        desc = Name { nameLocalName = T.pack "description", nameNamespace = Nothing, namePrefix = Nothing }
        params = Name { nameLocalName = T.pack "parameters", nameNamespace = Nothing, namePrefix = Nothing }
        rules = Name { nameLocalName = T.pack "rules", nameNamespace = Nothing, namePrefix = Nothing }
        gen = Name { nameLocalName = T.pack "generations", nameNamespace = Nothing, namePrefix = Nothing }
        len = Name { nameLocalName = T.pack "length", nameNamespace = Nothing, namePrefix = Nothing }
        coloring = Name { nameLocalName = T.pack "coloring", nameNamespace = Nothing, namePrefix = Nothing }
        typeAtt = Name { nameLocalName = T.pack "type", nameNamespace = Nothing, namePrefix = Nothing }
        palette = Name { nameLocalName = T.pack "palette", nameNamespace = Nothing, namePrefix = Nothing }
        color = Name { nameLocalName = T.pack "color", nameNamespace = Nothing, namePrefix = Nothing }
        algorithm = Name { nameLocalName = T.pack "algorithm", nameNamespace = Nothing, namePrefix = Nothing }
        continuous = Name { nameLocalName = T.pack "continuous", nameNamespace = Nothing, namePrefix = Nothing }
        vec1desc = el >>= descendant
        info1 = vec1desc >>= C.element info >>= child
        name1 = info1 >>= C.element nm >>= child >>= content
        desc1 = info1 >>= C.element nm >>= child >>= content
        params1 = vec1desc >>= C.element params >>= child
        rules1 = vec1desc >>= C.element rules
        rulesType1 = rules1 >>= C.attribute typeAtt
        rules1Continuous = rules1 >>= C.attribute continuous
        rulesContent1 = rules1 >>= child >>= content
        gen1 = params1 >>= C.element gen >>= child >>= content
        genMaybe = readMaybe (head $ map T.unpack gen1) :: Maybe Int
        generations = maybe 1 id genMaybe
        len1 = params1 >>= C.element len >>= child >>= content
        lenMaybe = readMaybe (head $ map T.unpack len1) :: Maybe Double
        length = maybe 1.0 id lenMaybe
        coloring1 = params1 >>= C.element coloring >>= child
        palette1 = coloring1 >>= C.element palette >>= child
        color1 = palette1 >>= C.element color >>= child
        colorName1 = color1 >>= C.element nm >>= child >>= content
        algorithm1 = coloring1 >>= C.element algorithm
        algType = algorithm1 >>= C.attribute typeAtt
        algUnpacked = map T.unpack algType
        alg = if null algUnpacked then "level" else head algUnpacked
        contUnpacked = map T.unpack rules1Continuous
        cont = if null contUnpacked then True else head contUnpacked == "continuous"
        colorAlg = ctorColoringAlgorithm alg algorithm1
    in
        XmlObj 
            { xobName = head $ map T.unpack name1
            , xobDesc = head $ map T.unpack desc1
            , xobGenerations = generations
            , xobLength = length
            , xobRules = head $ map T.unpack rulesContent1
            , xobBuiltIn = "builtin" == (head $ map T.unpack rulesType1)
            , xobColors = map T.unpack colorName1
            , xobAlgorithm = maybe (CalLevel 1) id colorAlg
            , xobContinuous = cont
            }

parseXmlVector :: String -> IO [XmlObj]
parseXmlVector fname = do
    doc <- readFile def fname
    let cursor = fromDocument doc
        vector = Name { nameLocalName = T.pack "vector", nameNamespace = Nothing, namePrefix = Nothing }
        vecs = child cursor >>= C.element vector
        plotObjects = map (\x -> ctorXmlObj [x]) vecs
        po1 = head plotObjects 
    putStrLn ("xml obj = " ++ (show po1))
    return plotObjects

-- //namespace vanmeule.FSharp.PlottingAndScheming

-- (* Copyright (c) 2009, 2008, 2007, 2006 by André van Meulebrouck.  All rights reserved worldwide. *)

-- //new vs. old
-- module PlottingAndScheming.Vector

-- open PlottingAndScheming.Scheme
-- open PlottingAndScheming.xml
-- open System
-- open System.Collections.Generic
-- open System.Windows
-- open System.Windows.Controls
-- open System.Windows.Media
-- open System.Windows.Shapes
-- open System.Xml.Linq

-- (*
-- to do:

-- 0) lambda closure problem
-- 1) back up to old
-- 2) convert drawf into record
-- 3) add counter to it
-- 4) add coloring algorithm based on vector line count (using modulo math)
-- 5) work out the math to generalize it
-- 6) force:  force a certain color
-- 7) filter out duplicate vectors
-- 9) clean up lisp code
-- 11) sort combo box entries
-- 12) status pane for status information with tabs (warnings, errors, info)
--     disable tabs if empty
--     use info for window title
-- 13) hilbert (can't do this one using, except maybe via adding generation parameter)

-- *)

data PalettePicker = PalettePicker
    { ppkPalette :: [Color] --this comes from xml; to do:  change to array
    , ppkLoc :: Int }

ctorPalettePicker :: [Color] -> PalettePicker
ctorPalettePicker colors =
    PalettePicker { ppkPalette = colors, ppkLoc = -1 }

paletteColor :: PalettePicker -> Color --to do:  redo as an array
paletteColor (PalettePicker { ppkPalette = p, ppkLoc = l }) = 
    p !! l

paletteInc :: PalettePicker -> PalettePicker
paletteInc (PalettePicker { ppkPalette = pp, ppkLoc = l }) = 
    PalettePicker { ppkPalette = pp, ppkLoc = (l + 1) `mod` (length pp)}

paletteSetLoc :: PalettePicker -> Int -> PalettePicker
paletteSetLoc pp i =
    if i < 0 then
        error "index cannot be negative:  for -1, use reset"
    else
        let palette = ppkPalette pp
        in 
            PalettePicker { ppkPalette = palette, ppkLoc = i `mod` (length palette) } 


-- tailPalette :: PalettePicker -> PalettePicker --to do:  redo
-- tailPalette (PalettePicker { ppkPalette = p, ppkLoc = l }) = 
--     PalettePicker { ppkPalette = p, ppkLoc = l }

-- type palettePicker = {
--     palette : Color array; 
--     mutable loc : int; } with
--     static member create (p : Color array) = {        
--         palette = 
--             if p.Length = 0 then
--                 failwith "the palette cannot be empty"
--             else p;
--         loc = -1; }
--     member x.color 
--         with get () =
--             if x.loc > -1 then
--                 x.palette.[x.loc]
--             else 
--                 failwith "attempt to reference color when color wasn't set"
--     member x.index 
--         with get () = 
--             x.loc
--         and set i =
--             if i < 0 then
--                 failwith "index cannot be negative:  for -1, use reset"
--             else
--                 x.loc <- i % x.palette.Length
--     member x.inc () =
--         x.loc <- (x.loc + 1) % x.palette.Length
--     member x.reset () =
--         x.loc <- -1

data SizeColorizer = SizeColorizer 
    { sczPicker :: PalettePicker
    , sczNumberOfRules :: Int
    , sczSubtractor :: Int
    , sczGenerations :: Int
    , sczRuleCount :: Int
    , sczSizes :: [Double]
    , sczCounter :: Int
    }

ctorSizeColorizer :: PalettePicker -> Int -> Int -> Int -> SizeColorizer
ctorSizeColorizer pp numRules subtractor generations = 
    let exp = generations - subtractor
    in 
        if exp < 0 then
            error "subtractor should not exceed generations"
        else
            SizeColorizer 
                { sczPicker = pp
                , sczNumberOfRules = numRules ^ (generations - subtractor)
                , sczSubtractor = subtractor
                , sczGenerations = generations
                , sczRuleCount = numRules
                , sczSizes = []
                , sczCounter = 0
                }

checkSizeColorizer :: SizeColorizer -> Int -> Double -> SizeColorizer
checkSizeColorizer sc i size =
    if i `mod` (sczNumberOfRules sc) == 0 then
        -- check to see if we know about this size already
        case findIndex (==size) (sczSizes sc) of
            Just x -> 
                --use size index to set the index of the color picker
                let pp = paletteSetLoc (sczPicker sc) x
                in sc { sczPicker = pp }
            Nothing -> 
                --add newly found size
                let newSizes = (sczSizes sc) ++ [size]
                in sc 
                    { sczSizes = (sczSizes sc) ++ [size]
                    , sczPicker = paletteSetLoc (sczPicker sc) ((length newSizes) -1) }
    else
        sc

colorSizeColorizer :: SizeColorizer -> Color
colorSizeColorizer clz =
    paletteColor $ sczPicker clz

-- //to do:  could put start index into the mix, retrieve from xml
-- type sizeColorizer = {
--     numberOfRules : int;
--     picker : palettePicker;
--     subtractor : int;
--     generations : int;
--     ruleCount : int;
--     mutable sizes : double list;
--     mutable counter : int;
--     } with
--     static member create numRules picker subtractor generations = 
--         let exp = generations - subtractor
--         if exp < 0 then
--             failwith "subtractor should not exceed generations"
--         else {
--             ruleCount = numRules;
--             numberOfRules = pown numRules (generations - subtractor);
--             picker = picker;
--             subtractor = subtractor;
--             generations = generations;
--             sizes = [];
--             counter = 0; }
--     member x.check i size =        
--         if i % x.numberOfRules = 0 then 
--             //check to see if we know about this size already
--             let find = List.tryFindIndex (fun el -> el = size) x.sizes
--             match find with 
--             | Some i -> 
--                 //use size index to set the index of the color picker
--                 x.picker.index <- i
--             | None -> 
--                 //add newly found size
--                 x.sizes <- x.sizes @ [size]
--                 x.picker.index <- x.sizes.Length - 1
 
data LevelColorizer = LevelColorizer
    { lczPicker :: PalettePicker 
    , lczLevel :: Int
    }

--to do:  create method of levelColorizer

ctorLevelColorizer :: PalettePicker -> Int -> Int -> LevelColorizer
ctorLevelColorizer pp lvl gen = 
    LevelColorizer { lczPicker = pp, lczLevel = lvl `mod` (gen + 1) }
 
-- type levelColorizer = {
--     picker : palettePicker; 
--     level : int; } with
--     static member create (picker : palettePicker) level generations = {
--         picker = picker;
--         level = level % (generations + 1) }
--     member x.check level =
--         if x.level = level then
--             x.picker.inc ()

--to do:  need picker inc method

checkLevel :: LevelColorizer -> Int -> LevelColorizer
checkLevel l lvl = 
    if lczLevel l == lvl then
        let palette' = paletteInc $ lczPicker l
        in LevelColorizer { lczPicker = palette', lczLevel = lczLevel l }
    else
        l

data VectorColorizer = 
    VczLevel LevelColorizer |
    VczImage SizeColorizer

-- type vectorColorizer = 
--     | Level of levelColorizer
--     | Image of sizeColorizer
--     //| Solid of int
--     //| Group of int
--     //| Force

--to do:  bookmark here

data DrawF = DrawF 
    { lines :: [(UI.Point, UI.Point)]
    , color :: UI.Color }

-- //to do:  push function
-- //keep track of number of line segments
-- //coloring algorithm
-- //first, just match existing functionality
-- type drawf = {
--     lines : Stack<Line>;
--     color : Color; 
--     //stroke thickness
--     } with 
--     static member create () = 
--         ()
--     member x.push x1 y1 x2 y2 = 
--         ()
    
-- type vecLen = //length -> computed length
--     | BuiltIn of (double -> double)
--     | Scheme of scmBlock

data VecLen =
    VlnBuiltIn (Double -> Double) |
    VlnScheme  ScmObject
    
-- type vecAngle = //angle -> flip rule -> computed angle
--     | BuiltIn of (double -> int -> double) 
--     | Scheme of scmBlock

data VecAngle =
    VanBuiltIn (Double -> Int -> Double) |
    VanScheme ScmObject

-- //to do:  should be length -> angle -> flip -> origin -> computed origin
-- type vecOrigin = //length -> angle -> origin -> flip -> computed origin
--     | BuiltIn of (double -> double -> (double * double) -> int -> (double * double)) 
--     | Scheme of scmBlock

data VecOrigin =
    VorBuiltIn (Double -> Double -> (Double, Double) -> Int -> (Double, Double)) |
    VorScheme ScmObject

-- type vecFlip =
--     | BuiltIn of int
--     | Scheme of scmAtom
    
data VecFlip =
    FlpBuiltIn Int |
    FlpScheme ScmObject

-- type vectorRule = {
--     lenf : vecLen;
--     anglef : vecAngle;
--     originf : vecOrigin;
--     flipAngle : vecFlip;
--     flipRules : vecFlip; }

data VecRule = VecRule 
    { vrlLenf :: VecLen
    , vrlAnglef :: VecAngle
    , vrlOriginf :: VecOrigin
    , vrlFlipAngle :: VecFlip
    , vrlFlipRules :: VecFlip
    }

mandelbrotPeanoCurveIntervals13 :: (VecRule, [VecRule])
mandelbrotPeanoCurveIntervals13 = 
    let project1of2 x y = x
        project3of4 w x y z = y
        lenDiv3 len = len / 3.0
        degrees60 = pi / 3.0
        anglePlus60 angle flip = angle + (degrees60 * (fromIntegral flip))
        degreesLess60 angle flip = angle - (degrees60 * (fromIntegral flip))
        fivePiOver6 = 5.0 * pi / 6.0
        anglePlus5PiOver6 angle flip = angle + (fivePiOver6 * (fromIntegral flip))
        angleLess5PiOver6 angle flip = angle - (fivePiOver6 * (fromIntegral flip))
        lenDiv3sqrt3 len = len / (3.0 * sqrt 3.0)
        angleLessPiHalves angle flip = angle - ((fromIntegral flip) * pi / 2.0)
        initiator = VecRule 
            { vrlLenf = VlnBuiltIn id
            , vrlAnglef = VanBuiltIn project1of2
            , vrlOriginf = VorBuiltIn project3of4
            , vrlFlipAngle = FlpBuiltIn 1
            , vrlFlipRules = FlpBuiltIn 1
            }
        generator = 
            [ VecRule --rule 1 (list len-div-3 angle-plus-60 project3.4 -1 1) ; 1
                { vrlLenf = VlnBuiltIn id
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn 1
                , vrlFlipRules = FlpBuiltIn 1
                }
            , VecRule --rule 2 (list len-div-3 angle-plus-60 project3.4 1 1) ; 2
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn anglePlus60
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }
            , VecRule --rule 3 (list len-div-3 project1.2 project3.4 1 1) ; 3
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn anglePlus60
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn 1
                , vrlFlipRules = FlpBuiltIn 1
                }
            , VecRule --rule 4 (list len-div-3 angle-less-60 project3.4 1 1) ; 4
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn anglePlus60
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }
            , VecRule --rule 5 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 1 1) ; 5
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn anglePlus5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }
            , VecRule --rule 6 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 -1 1) ; 6
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn anglePlus5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }
            , VecRule --rule 7 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 7
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLess5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }                              
            , VecRule --rule 8 (list len-div-3sqrt3 angle-less-pi-halves project3.4 -1 1) ; 8
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLessPiHalves
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }  
            , VecRule --rule 9 (list len-div-3 project1.2 project3.4 1 1) ; 9
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn 1
                , vrlFlipRules = FlpBuiltIn 1
                }  
            , VecRule --rule 10 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 1 1) ; 10
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLess5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn 1
                , vrlFlipRules = FlpBuiltIn 1
                }  
            , VecRule --rule 11 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 11
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLess5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }  
            , VecRule --rule 12 (list len-div-3 project1.2 project3.4 -1 1) ; 12
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn (-1)
                , vrlFlipRules = FlpBuiltIn 1
                }  
            , VecRule --rule 13 (list len-div-3 project1.2 project3.4 1 1)) ; 13
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = FlpBuiltIn 1
                , vrlFlipRules = FlpBuiltIn 1
                }            
            ]
    in
        (initiator, generator)

-- //to do:
-- (*
-- step 2:  parse rules from xml
-- step 3:  create scheme rules
-- step 4:  change plotter to match on type of rule
-- *)

-- let seed, rules = 
--     let identity x = x
--     let project1of2 x y = x
--     let project3of4 w x (y : (double * double)) z = y
--     let lenDiv3 len = len / 3.0
--     let degrees60 = Math.PI / 3.0
--     let anglePlus60 angle flip = angle + (degrees60 * (double flip))
--     let angleLess60 angle flip = angle - (degrees60 * (double flip))
--     let fivePiOver6 = 5.0 * Math.PI / 6.0
--     let anglePlus5PiOver6 angle flip = angle + (fivePiOver6 * (double flip))
--     let angleLess5PiOver6 angle flip = angle - (fivePiOver6 * (double flip))
--     let lenDiv3sqrt3 len = len / (3.0 * sqrt 3.0)
--     let angleLessPiHalves angle flip = angle - ((double flip) * Math.PI / 2.0)                
--     let initiator = [ 
--         //(list identity project1.2 project3.4 1 1)
--          {  lenf = vecLen.BuiltIn identity;
--             anglef = vecAngle.BuiltIn project1of2; 
--             originf = vecOrigin.BuiltIn project3of4; 
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1;
--             }]  
--     let generator = [
--         //rule 1 (list len-div-3 angle-plus-60 project3.4 -1 1) ; 1
--         {   lenf = vecLen.BuiltIn lenDiv3; 
--             anglef = vecAngle.BuiltIn anglePlus60;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn -1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 2 (list len-div-3 angle-plus-60 project3.4 1 1) ; 2
--         {   lenf = vecLen.BuiltIn lenDiv3; 
--             anglef = vecAngle.BuiltIn anglePlus60;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1;  
--             };
--         //rule 3 (list len-div-3 project1.2 project3.4 1 1) ; 3
--         {   lenf = vecLen.BuiltIn lenDiv3; 
--             anglef = vecAngle.BuiltIn project1of2;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 4 (list len-div-3 angle-less-60 project3.4 1 1) ; 4
--         {   lenf = vecLen.BuiltIn lenDiv3; 
--             anglef = vecAngle.BuiltIn angleLess60;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1;  
--             };
--         //rule 5 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 1 1) ; 5
--         {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
--             anglef = vecAngle.BuiltIn anglePlus5PiOver6;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1;  
--             };
--         //rule 6 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 -1 1) ; 6
--         {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
--             anglef = vecAngle.BuiltIn anglePlus5PiOver6;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn -1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 7 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 7
--         {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
--             anglef = vecAngle.BuiltIn angleLess5PiOver6;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn -1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 8 (list len-div-3sqrt3 angle-less-pi-halves project3.4 -1 1) ; 8
--         {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
--             anglef = vecAngle.BuiltIn angleLessPiHalves;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn -1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 9 (list len-div-3 project1.2 project3.4 1 1) ; 9
--         {   lenf = vecLen.BuiltIn lenDiv3; 
--             anglef = vecAngle.BuiltIn project1of2;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 10 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 1 1) ; 10
--         {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
--             anglef = vecAngle.BuiltIn angleLess5PiOver6;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 11 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 11
--         {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
--             anglef = vecAngle.BuiltIn angleLess5PiOver6;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn -1;
--             flipRules = vecFlip.BuiltIn 1;
--             };
--         //rule 12 (list len-div-3 project1.2 project3.4 -1 1) ; 12
--         {   lenf = vecLen.BuiltIn lenDiv3; 
--             anglef = vecAngle.BuiltIn project1of2;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn -1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--         //rule 13 (list len-div-3 project1.2 project3.4 1 1)) ; 13
--         {   lenf = vecLen.BuiltIn lenDiv3; 
--             anglef = vecAngle.BuiltIn project1of2;
--             originf = vecOrigin.BuiltIn project3of4;
--             flipAngle = vecFlip.BuiltIn 1;
--             flipRules = vecFlip.BuiltIn 1; 
--             };
--             ]
--     initiator, generator

--to do:  not highest priority, but must be done

-- let xmlPalette (xml : XElement) =
--     xml.Elements ()
--     |> Seq.map                    
--         (fun e -> 
--             let el = e.FirstNode :?> XElement
--             let name = el.Name.LocalName
--             match name with
--             | "name" -> 
--                 ColorConverter.ConvertFromString(el.Value) :?> Color
--             | "rgb" ->
--                 let r = el.Element (xname "red")
--                 let g = el.Element (xname "green")
--                 let b = el.Element (xname "blue")
--                 let r = Convert.ToByte (r.Value)
--                 let g = Convert.ToByte (g.Value)
--                 let b = Convert.ToByte (b.Value)
--                 let clr = Color.FromRgb(r, g, b)
--                 clr
--             | _ -> 
--                 failwith "bad color specification") 
--     |> Seq.toArray

-- data PlotObject = PlotObject --to do:  need XmlObj here
--     { initiator :: [VecRule]
--     , generator :: [VecRule]
--     , continuous :: Bool --in XmlObj
--     , generations :: Int --in XmlObj
--     , poLength :: Double --in XmlObj
--     , coloring :: VectorColorizer
--     -- , xml :: String
--     }

selectBuiltin :: String -> Maybe (VecRule, [VecRule])
selectBuiltin nm = 
    if nm == "mandelbrotPeanoCurveIntervals13" then 
        Just mandelbrotPeanoCurveIntervals13 
    else 
        Nothing

--to do:  create function

-- ctorPlotObject :: String -> PlotObject --to do:  should take XmlObj?
-- ctorPlotObject xml = 

--     undefined

-- type plotObject = {
--     xml : XElement; 
--     initiator : vectorRule list;
--     generator : vectorRule list; 
--     generations : int; 
--     length : float; 
--     coloring : vectorColorizer; 
--     continuous : bool; } with 

{-
main :: IO ()
main = do
    doc <- readFile def "PlottingAndScheming/xml/vector.xml" 
    let cursor = fromDocument doc
        vector = Name { nameLocalName = T.pack "vector", nameNamespace = Nothing, namePrefix = Nothing }
        info = Name { nameLocalName = T.pack "info", nameNamespace = Nothing, namePrefix = Nothing }
        nm = Name { nameLocalName = T.pack "name", nameNamespace = Nothing, namePrefix = Nothing }
    -- print $ T.concat $ 
        -- plots = child cursor >>= C.element vector >>= child >>= descendant >>= content
        -- plots = child cursor >>= C.element vector >>= child >>= C.element info >>= child >>= descendant >>= content
        plots = child cursor >>= C.element vector >>= child >>= C.element info >>= child >>= C.element nm >>= child >>= content
        -- plots = child cursor >>= C.element vector >>= child >>= descendant >>= C.element info
        -- names = plots >>= child >>= C.element info >>= child >>= descendant >>= content
        -- plots = child cursor >>= C.element info >>= child >>= descendant >>=  content
    putStrLn $ show plots
    startGUI defaultConfig $ setup $ map T.unpack plots
-}

--below created plot obj

-- vectorXml :: String -> 
--     static member create (xml : XElement) =
--         let parameters = xml.Element (xname "parameters")
--         let generations = PlottingAndScheming.xml.fetchInt parameters "generations"
--         let generations = 
--             match generations with
--             | Some i -> i
--             | _ -> failwith "bad generations specified in xml"
--         let len = PlottingAndScheming.xml.fetchDouble parameters "length"
--         let len = 
--             match len with
--             | Some f -> f
--             | _ -> failwith "bad length specified in xml"
--         let coloring = PlottingAndScheming.xml.fetchElement parameters "coloring"
--         let palette = PlottingAndScheming.xml.fetchElement coloring "palette"
--         let colors = xmlPalette palette
--         let algorithm = PlottingAndScheming.xml.fetchElement coloring "algorithm"
--         let algorithmType = PlottingAndScheming.xml.fetchAttribute algorithm "type"
--         let picker = palettePicker.create colors
--         let code = xml.Element (xname "rules")
--         let builtin = code.Attribute (xname "type")
--         let builtin = 
--             if builtin = null then
--                 "builtin"
--             else 
--                 builtin.Value.ToString ()
--         let continuous = 
--             let att = code.Attribute (xname "continuous") 
--             if att = null then
--                 true
--             else 
--                 (att.Value.ToString ()) = "yes"  
--         let seed, rules =
--             match builtin with
--             | "builtin" -> 
--                 seed, rules
--             | _ -> 
--                 let code = code.Value
--                 let code = code.Replace("\n", "\r\n")               
--                 let evaled = evalString code
--                 let dump = printHeap evaled
--                 let e = Some evaled
--                 let initiator, generator =
--                     funCar e, funCadr e
--                 //break up LISP expressions into F# records
--                 //walk down into rules
--                 let createRules rules = 
--                     let rec iter rules records =
--                         if funNull rules then
--                             List.rev records
--                         else
--                             let rule = Some (funCar rules)
--                             //walk across a rule
--                             let lenf = funCar rule
--                             let rule = funCdr rule
--                             let anglef = funCar rule
--                             let rule = funCdr rule
--                             let originf = funCar rule
--                             let rule = funCdr rule
--                             let flipAngle = funCar rule
--                             let flipa = (fromScheme flipAngle) :?> int               
--                             let rule = funCdr rule
--                             let flipRules = funCar rule
--                             let flipr = (fromScheme flipRules) :?> int
--                             let record = 
--                                 match lenf, anglef, originf, flipAngle, flipRules with
--                                 |   scmObject.Block l, 
--                                     scmObject.Block a,
--                                     scmObject.Block o,
--                                     scmObject.Atom (scmAtom.Int ia),
--                                     scmObject.Atom (scmAtom.Int ir) -> 
--                                     {   lenf = vecLen.Scheme l;
--                                         anglef = vecAngle.Scheme a;
--                                         originf = vecOrigin.Scheme o;
--                                         //originf = vecOrigin.BuiltIn (fun l a o f -> o);
--                                         flipAngle = 
--                                             vecFlip.BuiltIn flipa;
--                                         flipRules = 
--                                             vecFlip.BuiltIn flipr;
--                                         }
--                                 | _ -> failwith "bad rule specification"
--                             iter (funCdr rules) (record :: records)
--                     iter rules []
--                 let seed = createRules (Some initiator)
--                 let rules = createRules (Some generator)
--                 seed, rules
--         let colorizer =
--             match algorithmType with
--             | Some "image" -> 
--                 let subtractor = 
--                     let el = PlottingAndScheming.xml.fetchInt algorithm "subtractor"
--                     match el with
--                     | Some i -> i
--                     | None -> failwith "subtractor was not specified for images coloring algorithm"
--                 vectorColorizer.Image 
--                     (sizeColorizer.create rules.Length picker subtractor generations)
--             | Some "level" ->
--                 let level = 
--                     let el = PlottingAndScheming.xml.fetchInt algorithm "level"
--                     match el with
--                     | Some i -> i
--                     | None -> failwith "bad level argument for level colorizer"
--                 vectorColorizer.Level (levelColorizer.create picker level generations)
--             | _ -> failwith "unknown colorizing algorithm"
--         {   xml = xml; 
--             initiator = seed;
--             generator = rules; 
--             generations = generations; 
--             length = len; 
--             coloring = colorizer; 
--             continuous = continuous; } 

-- let vectorEndPoint vec len angle =
--     (fst vec) + (len * (Math.Cos angle)),
--     (snd vec) + (len * (Math.Sin angle))

vectorFractal :: XmlObj -> [Vector]
vectorFractal xob =
    --to do:  check for builtin true, then get builtin
    let (seed, rules) = mandelbrotPeanoCurveIntervals13
        colors = xobColors xob
        pp = ctorPalettePicker colors
        colorAlg = (xobAlgorithm xob)
        numRules = length rules
        gen = (xobGenerations xob)
        colorizer =
            case colorAlg of
                CalLevel lvl -> 
                    VczLevel (ctorLevelColorizer pp lvl gen)
                CalImage subtractor -> 
                    VczImage (ctorSizeColorizer pp numRules subtractor gen)
        drawf :: [Vector] -> VectorColorizer -> Double -> Double -> UI.Point -> (UI.Point, [Vector], VectorColorizer)
        drawf vecs clz len angle origin =
            let (x, y) = origin
                pt2 = (x + len * cos angle, y + len * sin angle)
                ln = (origin, pt2)
                idx = length vecs
                chk p i j n = i >= n * p && i < (n + j) * p
                clz' = --possibly update colorizer
                    case clz of 
                        l@(VczLevel _) -> l
                        VczImage s -> VczImage (checkSizeColorizer s idx len)
                clr = --pick color from colorizer
                    case clz' of
                        VczLevel l -> paletteColor $ lczPicker l
                        VczImage s -> paletteColor $ sczPicker s
                vec = Vector { vecP1 =  origin, vecP2 = pt2, vecColor = clr }
                vecs' = vec : vecs
            in 
                (pt2, vecs', clz')
        twoDvectorFractal :: --to do:  determine if drawf needs to be passed:  it's visible via scope
            -- ([Vector] -> VectorColorizer -> Double -> Double -> (Double, Double) -> ((Double, Double), [Vector], VectorColorizer)) -> --drawf
            [VecRule] -> --seed
            [VecRule] -> --rules
            Double -> --len
            Double -> --angle
            Double -> --xorigin
            Double -> --yorigin
            (Double -> Int -> Bool) -> --quitf --to do:  determine if this needs to be passed as an argument
            ((Double, Double), [Vector], VectorColorizer) --origin (return value)
        twoDvectorFractal
            -- drawf 
            seed
            rules
            len
            angle
            xOrigin
            yOrigin
            quitf
            =  
            -- //change mapvector to be non-recursive 
            -- //it will record the end of the vector
            -- //it will return this end of the vector when the vector is done being walked
            -- //inside mapvector will be a recursive function that will do the walking
            -- //do a backup before doing this!!!!
            -- //to do:  instead of len, angle, origin; create a vector class and use real vectors
            let across :: Double -> Double -> UI.Point -> Int -> Int -> Int -> [VecRule] -> [Vector] -> VectorColorizer -> (UI.Point, [Vector], VectorColorizer)
                across 
                    len 
                    angle 
                    origin
                    flipAngleFactor
                    flipRulesFactor
                    generation
                    vector
                    vectors
                    colorizers
                    = 
                    if null vector then 
                        (origin, [], colorizer)
                    else 
                        let (currentSeed : restSeed) = vector
                            lenf = 
                                case (vrlLenf currentSeed) of
                                    VlnBuiltIn f -> f len
                                    VlnScheme g -> --to do phase 2
                                        undefined
                                        -- let res = apply g
                            anglef =
                                case (vrlAnglef currentSeed) of
                                    VanBuiltIn f -> f angle flipAngleFactor
                                    VanScheme s -> undefined 
                            originf = 
                                let (newOrigin, vecs, colorizer) =                             
                                        across 
                                            len 
                                            angle 
                                            origin 
                                            flipAngleFactor
                                            flipRulesFactor
                                            generation 
                                            restSeed
                                            vectors
                                            colorizer
                                in 
                                    case (vrlOriginf currentSeed) of
                                        VorBuiltIn f -> 
                                            let origin = f len angle newOrigin flipAngleFactor
                                            in (origin, vecs, colorizer)
                                        VorScheme g -> 
                                            undefined
                            flipAngle = 
                                case (vrlFlipAngle currentSeed) of
                                    FlpBuiltIn d -> d
                                    FlpScheme s -> error "flip angle not implemented yet"                            
                            flipRules = 
                                case (vrlFlipRules currentSeed) of
                                    FlpBuiltIn d -> d
                                    FlpScheme s -> error "flip rules not implemented yet"
                        in --undefined
                            down 
                                lenf
                                anglef
                                (0, 0) -- (fst originf)
                                (flipAngleFactor * flipAngle)
                                (flipRulesFactor * flipRules)
                                (generation + 1)
                                vectors
                                colorizer
                down :: Double -> Double -> (Double, Double) -> Int -> Int -> Int -> [Vector] -> VectorColorizer -> ((Double, Double), [Vector], VectorColorizer)
                down len angle origin flipAngleFactor flipRulesFactor generation vectors colorizer =
                    let colorizer' =
                            case colorizer of
                                VczLevel l -> 
                                    VczLevel $ checkLevel l generation
                                otherwise -> colorizer
                    in 
                        if quitf len generation then --to do vecs clz len angle origin
                            drawf vectors colorizer len angle origin
                            -- let res = drawf len angle origin
                            -- in res
                        else
                            undefined
--             match colorizer with
--                 | vectorColorizer.Level l ->
--                     l.check (int generation)
--                     ()
--                 | _ -> 
--                     ()
--             if quitf len generation then
--                 (drawf 
--                     len 
--                     angle 
--                     origin)
--             else 
--                 let rules = 
--                     if flipRulesFactor > 0 then 
--                         (List.rev rules)
--                     else
--                         rules
--                 let newOrigin =
--                     across
--                         len
--                         angle
--                         origin
--                         flipAngleFactor
--                         flipRulesFactor
--                         generation
--                         rules
--                 if plotobj.continuous then
--                     newOrigin
--                 else 
--                     vectorEndPoint origin len angle                    
                    -- undefined --to do
            in
                across
                    len 
                    angle 
                    (xOrigin, yOrigin) -- origin
                    1 -- angle flip
                    1 -- rule flip
                    (-1) -- generation
                    (reverse seed)
                    []
                    colorizer
        quitf generations = 
            if generations < 0 then
                undefined --to do
                -- fun (len : double) (generation : int) -> len < (double generations)
            else 
                undefined --to do
                -- fun (len : double) (generation : int) -> generation = generations                 

    in
        let len = xobLength xob
            generations = xobGenerations xob
        in 
            -- let (_, vectors, _) = 
            --         twoDvectorFractal 
            --             -- drawf          
            --             [seed] --initiator
            --             rules --generator
            --             len --len
            --             0.0 --start angle
            --             0.0 --flip angle
            --             0.0 --flip rules
            --             (quitf generations)
            -- in vectors
            [ Vector { vecP1 = (30, 30), vecP2 = (20, 20), vecColor = "blue" }
            , Vector { vecP1 = (20, 20), vecP2 = (80, 20), vecColor = "grey" }
            , Vector { vecP1 = (80, 20), vecP2 = (50, 50), vecColor = "orange" }
            , Vector { vecP1 = (0, 50), vecP2 = (200, 150), vecColor = "maroon" }
            ]

-- let vectorFractal (plotobj : plotObject) =
--     let seed = plotobj.initiator
--     let rules = plotobj.generator
--     let colorizer = plotobj.coloring 
--     let drawf 
--         (lines : Stack<Line>) 
--         (len : double) 
--         (angle : double)
--         (origin : double * double) =
--         let x = fst origin
--         let y = snd origin
--         let ln = new Line()
--         ln.X1 <- x
--         ln.Y1 <- y
--         let x2 = x + len * Math.Cos angle
--         let y2 = y + len * Math.Sin angle
--         ln.X2 <- x2
--         ln.Y2 <- y2
--         let idx = lines.Count
--         let chk p i j n = i >= n * p && i < (n + j) * p
--         let gen = plotobj.generations
--         (*
--         There are two coloring cases: 
--             1) the same diagram repeats at different sizes
--             2) all diagrams are the same size

--         For coloring type #1, look for the sizes of diagrams and color based on size.
--         For type #2, decide how many groups you want and color accordingly
--         *)
--         let clr = 
--             match colorizer with 
--             | vectorColorizer.Image i ->
--                 i.check idx len
--                 i.picker.color
--             | vectorColorizer.Level l ->
--                 l.picker.color               
--         ln.Stroke <- new SolidColorBrush(clr)
--         ln.StrokeThickness <- 2.0
--         lines.Push ln
--         (x2, y2)
--     let twoDvectorFractal 
--         drawf 
--         (seed : list<vectorRule>) 
--         (rules : list<vectorRule>) 
--         (len : double)
--         (angle : double)
--         (xorigin : double)
--         (yorigin : double)
--         (quitf : double -> int -> bool)
--         =  
--         //change mapvector to be non-recursive 
--         //it will record the end of the vector
--         //it will return this end of the vector when the vector is done being walked
--         //inside mapvector will be a recursive function that will do the walking
--         //do a backup before doing this!!!!
--         //to do:  instead of len, angle, origin; create a vector class and use real vectors      
--         let rec across 
--             len 
--             angle 
--             (origin : double * double) 
--             (flipAngleFactor : int) //to do:  change to int
--             (flipRulesFactor : int) //to do:  change to int
--             generation
--             (vector : list<vectorRule>) 
--             =            
--             if vector.IsEmpty then 
--                 origin                
--             else 
--                 let (currentSeed : vectorRule) = List.head vector
--                 let restSeed = List.tail vector
--                 let lenf =
--                     match currentSeed.lenf with
--                     | vecLen.BuiltIn f -> 
--                         (f len)
--                     | vecLen.Scheme g -> 
--                         let res = apply g ((toScheme len) :: [])
--                         let res = fromScheme res
--                         let res =
--                             match res with
--                             | :? int as i -> failwith "wanted a double"
--                             | :? double as d -> d
--                             | _ -> failwith "bad result from Scheme"
--                         res
--                 let anglef = 
--                     match currentSeed.anglef with
--                     | vecAngle.BuiltIn f -> 
--                         (f angle flipAngleFactor)
--                     | vecAngle.Scheme s -> 
--                         let args = (toScheme angle) :: (toScheme flipAngleFactor) :: []
--                         let res = apply s args
--                         let res = fromScheme res
--                         let res =
--                             match res with
--                             | :? double as d -> d
--                             | :? int as i -> 
--                                 (double i)
--                             | _ -> failwith "bad result from Scheme"
--                         res
--                 let originf =                    
--                     let newOrigin =                             
--                         across 
--                             len 
--                             angle 
--                             origin 
--                             flipAngleFactor
--                             flipRulesFactor
--                             generation 
--                             restSeed
--                     match currentSeed.originf with
--                     | vecOrigin.BuiltIn f -> 
--                         (f                       
--                             len 
--                             angle
--                             newOrigin
--                             flipAngleFactor)
--                     | vecOrigin.Scheme g -> 
--                         let args = 
--                             (toScheme len) ::
--                             (toScheme angle) ::
--                             (toScheme newOrigin) :: 
--                             (toScheme flipAngleFactor) :: 
--                             []
--                         let res = apply g args
--                         let res = fromScheme res
--                         let res =
--                             match res with
--                             | :? int as i -> failwith "wanted a double * double"
--                             | :? double as d -> failwith "wanted a double * double"
--                             | :? (double * double) as t -> t
--                             | _ -> failwith "bad result from Scheme"
--                         res
--                 let flipAngle = 
--                     match currentSeed.flipAngle with
--                     | vecFlip.BuiltIn d -> d
--                     | vecFlip.Scheme s -> failwith "not implemented yet"
--                 let flipRules = 
--                     match currentSeed.flipRules with
--                     | vecFlip.BuiltIn d -> d
--                     | vecFlip.Scheme s -> failwith "not implemented yet"
--                 down 
--                     lenf
--                     anglef
--                     originf
--                     (flipAngleFactor * flipAngle)
--                     (flipRulesFactor * flipRules)
--                     (generation + 1) 
--         and 
--             down 
--                 len 
--                 angle 
--                 (origin : double * double) 
--                 flipAngleFactor 
--                 flipRulesFactor 
--                 generation 
--                 =
--             match colorizer with
--                 | vectorColorizer.Level l ->
--                     l.check (int generation)
--                     ()
--                 | _ -> 
--                     ()
--             if quitf len generation then
--                 (drawf 
--                     len 
--                     angle 
--                     origin)
--             else 
--                 let rules = 
--                     if flipRulesFactor > 0 then 
--                         (List.rev rules)
--                     else
--                         rules
--                 let newOrigin =
--                     across
--                         len
--                         angle
--                         origin
--                         flipAngleFactor
--                         flipRulesFactor
--                         generation
--                         rules
--                 if plotobj.continuous then
--                     newOrigin
--                 else 
--                     vectorEndPoint origin len angle
--         across 
--             len 
--             angle 
--             (xorigin, yorigin) //origin
--             1 //angle flip
--             1 //rule flip
--             -1 //generation
--             (List.rev seed)
--     let quitf generations = 
--         if generations < 0 then
--             fun (len : double) (generation : int) -> len < (double generations)
--         else 
--             fun (len : double) (generation : int) -> generation = generations    
--     let win = new Window()
--     win.SizeToContent <- SizeToContent.WidthAndHeight
--     let xml = plotobj.xml
--     let title = 
--         let info = xml.Element (xname "info")
--         if info <> null then 
--             let name = info.Element (xname "name")
--             match name, plotobj.generations with 
--             | null, _ -> failwith "couldn't find the name"
--             | n, g -> n.Value + " [generation=" + g.ToString () + "]"
--         else
--             failwith "couldn't find the info node" 
--     win.Title <- title
--     let lines = new Stack<Line>()
--     let clr = Colors.Black
--     let generations = plotobj.generations
--     let len = plotobj.length
--     let _ = 
--         twoDvectorFractal 
--             (drawf lines)             
--             seed //initiator
--             rules //generator
--             len //len
--             0.0 //start angle
--             0.0 //flip angle
--             0.0 //flip rules
--             (quitf generations)
--     let can = new Canvas()  
--     //find extrema
--     let line = lines.Peek()
--     let loX : float ref = ref line.X1
--     let hiX : float ref = ref line.X1
--     let loY : float ref = ref line.Y1
--     let hiY : float ref = ref line.Y1
--     let checker x y =
--         if x < loX.Value then loX.Value <- x
--         if x > hiX.Value then hiX.Value <- x
--         if y < loY.Value then loY.Value <- y
--         if y > hiY.Value then hiY.Value <- y
--     for line in lines do
--         checker line.X1 line.Y1
--         checker line.X2 line.Y2
--     //set canvas size
--     can.Width <- hiX.Value - loX.Value
--     can.Height <- hiY.Value - loY.Value           
--     for line in lines do
--         //normalize lines
--         line.X1 <- line.X1 - loX.Value 
--         line.Y1 <- hiY.Value - line.Y1
--         line.X2 <- line.X2 - loX.Value
--         line.Y2 <- hiY.Value - line.Y2
--         //add line to canvas
--         can.Children.Add line |> ignore
--     let scroll = ScrollViewer()
--     scroll.Content <- can
--     scroll.HorizontalScrollBarVisibility <- ScrollBarVisibility.Auto
--     scroll.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto
--     win.Content <- scroll
--     win.Show()

--to do: this gets the xml from the dropdown, then calls vectorFractal with the plotObj for the given dropdown choice

-- let vectorPlot (txt : TextBox) =
--     (fun _ -> 
--         let txtXML = txt.Text
--         //to do:  put a try with pattern here
--         let xml = XElement.Parse txtXML
--         let plotObj = plotObject.create xml
--         vectorFractal plotObj |> ignore)

-- //to do:  reference xmlInfo and xmlEquation records
-- type vectorParameters = {
--     name : string; 
--     xml : XElement; }

-- let xname n = XName.op_Implicit (n)
-- let fetchElement (el : XElement) str = el.Element (xname str)

-- let populateCbx (cbx : ComboBox) xmlFolder =
--     cbx.DisplayMemberPath <- "name"
--     let xml = XDocument.Load(xmlFolder + @"\vector.xml")    
--     let plots = xml.Element(xname "plots")
--     let xelem s el = new XElement(xname s, box el)
--     let xatt a b = new XAttribute(xname a, b) |> box
--     let xstr s = box s     
--     //walk through xml elements and create records        
--     for elem in plots.Elements() do
--         //to do:  use xmlInfo and xmlEquation records
--         let info = elem.Element (xname "info")
--         let name = info.Element (xname "name")
--         let (param : vectorParameters) = { 
--             name = name.Value
--             xml = elem; }
--         cbx.Items.Add(param) |> ignore