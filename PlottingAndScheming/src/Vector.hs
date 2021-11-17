module Vector where

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS -Wall #-}

import Scheme
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
-- import Text.XML --avm:  test to see if this is necessary
import Prelude hiding (readFile, writeFile)
import Text.XML as X
import Text.XML.Cursor as C
import qualified Data.Text as T
import Text.Read
import Data.List
import Data.Maybe
import Data.Either

{-
Copyright (c) 2020, 2015, 2009, 2008, 2007, 2007 by André Marc van Meulebrouck.  All rights reserved worldwide.

File:  Vector.hs.
2020-08-21:  all fractals working, went surfing the next day
2020-08-19:  some lisp fractals working
2020-08-15:  builtin fractal working
-}

{-
to do:
    -> materialize seed, rule indexes as attributes in Vector data structure for post processing coloring
    seed would be passed as a parameter so any depth of recursion can know what seed it came from
    -> line record:  thickness = 2, color, p1, p2
    -> pass only index of colorizer?
    -> instead of incrementing the palette; reverse it (as a way to get a second palette?)
    -> change type attribute to builtin {yes, no}
-}

data Vector = Vector 
    { vecP1 :: UI.Point
    , vecP2 :: UI.Point
    , vecColor :: Color }
    deriving (Show)

type Color = String 

--to do:  move these to Main.hs?

setPixel :: UI.Point -> Color -> UI.Element -> UI () --to do:  pass color as UI.Color, make first arg
setPixel pt clr can = do
    -- c # set' UI.strokeStyle "yellow"
    can # set' UI.fillStyle (UI.htmlColor clr)
    can # UI.fillRect pt 1 1

drawVec :: Vector -> UI.Element -> UI ()
drawVec Vector { vecP1 = p1, vecP2 = p2, vecColor = clr } c = do
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
checkExtrema Vector { vecP1 = (x1, y1), vecP2 = (x2, y2) } Extremes { xLo = xLow, xHi = xHigh, yLo = yLow, yHi = yHigh } = 
    let (xlo, xhi) = checkPair x1 x2 (xLow, xHigh)
        (ylo, yhi) = checkPair y1 y2 (yLow, yHigh)
    in Extremes { xLo = xlo, xHi = xhi, yLo = ylo, yHi = yhi }

findExtrema :: [Vector] -> Extremes
findExtrema = 
    foldr checkExtrema (Extremes { xLo = Nothing, xHi = Nothing, yLo = Nothing, yHi = Nothing })
    
normalizeVectors :: [Vector] -> ([Vector], (Maybe Int, Maybe Int))
normalizeVectors vecs =
    case findExtrema vecs of
        Extremes { xLo = Just xl, xHi = Just xh, yLo = Just yl, yHi = Just yh } ->
            let width = xh - xl
                height = yh - yl
                adjust = \Vector { vecP1 = (x1, y1), vecP2 = (x2,y2), vecColor = c } -> 
                    let x1' = x1 - xl
                        y1' = yh - y1 --quadrant I
                        -- y1' = y1 - yl
                        x2' = x2 - xl
                        -- y2' = y2 - yl
                        y2' = yh - y2 --quadrant I
                    in Vector { vecP1 = (x1', y1'), vecP2 = (x2', y2'), vecColor = c }
                vecs' = map adjust vecs
            in (vecs', (Just $ ceiling width, Just $ ceiling height))
        _ -> 
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
    , xobContinuous :: Bool
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
        _ -> 
            Nothing
    where 
        findLevel :: Name -> Int
        findLevel nm =
            let lvl = el >>= child >>= C.element nm >>= child >>= content
                lvlUnpack = map T.unpack lvl
                lvlMaybe = if null lvlUnpack then Nothing else readMaybe (head lvlUnpack) :: Maybe Int
            in fromMaybe 1 lvlMaybe

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
        fractalName = if null name1 then "" else head $ map T.unpack name1
        desc1 = info1 >>= C.element desc >>= child >>= content
        description = if null desc1 then "" else head $ map T.unpack desc1
        params1 = vec1desc >>= C.element params >>= child
        rules1 = vec1desc >>= C.element rules
        rulesType1 = rules1 >>= C.attribute typeAtt
        rules1Continuous = rules1 >>= C.attribute continuous
        rulesContent1 = rules1 >>= child >>= content
        rulesValue = if null rulesContent1 then "" else head $ map T.unpack rulesContent1
        gen1 = params1 >>= C.element gen >>= child >>= content
        genMaybe = readMaybe (head $ map T.unpack gen1) :: Maybe Int
        generations = fromMaybe 1 genMaybe
        len1 = params1 >>= C.element len >>= child >>= content
        lenMaybe = readMaybe (head $ map T.unpack len1) :: Maybe Double
        length = fromMaybe 1.0 lenMaybe
        coloring1 = params1 >>= C.element coloring >>= child
        palette1 = coloring1 >>= C.element palette >>= child
        color1 = palette1 >>= C.element color >>= child
        colorName1 = color1 >>= C.element nm >>= child >>= content
        algorithm1 = coloring1 >>= C.element algorithm
        algType = algorithm1 >>= C.attribute typeAtt
        algUnpacked = map T.unpack algType
        alg = if null algUnpacked then "level" else head algUnpacked
        cont = 
            null rules1Continuous || "yes" == head (map T.unpack rules1Continuous)
        builtin =
            null rulesType1 || "builtin" == head (map T.unpack rulesType1)
        colorAlg = ctorColoringAlgorithm alg algorithm1
    in
        XmlObj 
            { xobName = fractalName
            , xobDesc = description
            , xobGenerations = generations
            , xobLength = length
            , xobRules = rulesValue
            , xobBuiltIn = builtin
            , xobColors = map T.unpack colorName1
            , xobAlgorithm = fromMaybe (CalLevel 1) colorAlg
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
    return plotObjects

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
paletteColor PalettePicker { ppkPalette = p, ppkLoc = l } = 
    p !! l

paletteInc :: PalettePicker -> PalettePicker
paletteInc PalettePicker { ppkPalette = pp, ppkLoc = l } = 
    PalettePicker { ppkPalette = pp, ppkLoc = (l + 1) `mod` length pp }

paletteSetLoc :: PalettePicker -> Int -> PalettePicker
paletteSetLoc pp i =
    if i < 0 then
        error "index cannot be negative:  for -1, use reset" --to do:  change this function into a maybe?
    else
        let palette = ppkPalette pp
        in 
            PalettePicker { ppkPalette = palette, ppkLoc = i `mod` length palette } 

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
            error "subtractor should not exceed generations" --to do:  get rid of this, convert to maybe
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
    if i `mod` sczNumberOfRules sc == 0 then
        -- check to see if we know about this size already
        case elemIndex size (sczSizes sc) of
            Just x -> 
                --use size index to set the index of the color picker
                let pp = paletteSetLoc (sczPicker sc) x
                in sc { sczPicker = pp }
            Nothing -> 
                --add newly found size
                let newSizes = sczSizes sc ++ [size]
                in sc 
                    { sczSizes = sczSizes sc ++ [size]
                    , sczPicker = paletteSetLoc (sczPicker sc) (length newSizes - 1) }
    else
        sc

colorSizeColorizer :: SizeColorizer -> Color
colorSizeColorizer clz =
    paletteColor $ sczPicker clz

data LevelColorizer = LevelColorizer
    { lczPicker :: PalettePicker 
    , lczLevel :: Int
    }

ctorLevelColorizer :: PalettePicker -> Int -> Int -> LevelColorizer
ctorLevelColorizer pp lvl gen = 
    LevelColorizer { lczPicker = pp, lczLevel = lvl `mod` (gen + 1) }
 
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

data DrawF = DrawF 
    { lines :: [(UI.Point, UI.Point)]
    , color :: UI.Color }

data VecLen =
    VlnBuiltIn (Double -> Double) |
    VlnScheme  ScmObject
    deriving (Show)
    
data VecAngle =
    VanBuiltIn (Double -> Int -> Double) |
    VanScheme ScmObject 
    deriving (Show)

data VecOrigin 
    = VorBuiltIn (Double -> Double -> UI.Point -> Int -> UI.Point) 
    | VorScheme ScmObject
    deriving (Show)
   
data VecRule = VecRule 
    { vrlLenf :: VecLen
    , vrlAnglef :: VecAngle
    , vrlOriginf :: VecOrigin
    , vrlFlipAngle :: Int
    , vrlFlipRules :: Int
    } deriving (Show)

ctorVecRule :: (Double -> Double, Double -> Int -> Double, Double -> Double -> UI.Point -> Int -> UI.Point, Int, Int) -> VecRule
ctorVecRule (lenf, anglef, originf, flipa, flipr) =
    VecRule 
        { vrlLenf = VlnBuiltIn lenf
        , vrlAnglef = VanBuiltIn anglef
        , vrlOriginf = VorBuiltIn originf
        , vrlFlipAngle = flipa
        , vrlFlipRules = flipr }

builtIns :: [ (String, ([VecRule], [VecRule]))]
builtIns = 
    [ ("beeHive", beeHive)
    , ("islands", islands)
    , ("islands2", islands2)
    , ("islandsAndLakes", islandsAndLakes)
    , ("mandelbrotPeanoCurveIntervals13", mandelbrotPeanoCurveIntervals13) 
    , ("sierpinskiCarpet", sierpinskiCarpet)
    , ("sierpinskiCarpet2", sierpinskiCarpet2)
    , ("sierpinskiGasket2", sierpinskiGasket2)
    , ("tiles", tiles)
    , ("tiles2", tiles2) ]

findBuiltin :: [ (String, ([VecRule], [VecRule])) ] -> String -> Maybe ([VecRule], [VecRule])
findBuiltin alist tgt =
    case find (\(c, _) -> c == tgt) alist of
        Just (_, v) -> Just v
        Nothing -> Nothing

project1of2 :: p1 -> p2 -> p1
project1of2 x y = x
project3of4 :: p1 -> p2 -> p3 -> p4 -> p3
project3of4 w x y z = y

beeHive :: ([VecRule], [VecRule])
beeHive = 
    let degrees30 = pi / 6.0
        cos30 = cos degrees30
        lenf len = len / 2.0 / cos30
        degrees90 = pi / 2.0
        degrees150 = 5 * degrees30
        anglePlus30 :: Double -> Int -> Double
        anglePlus30 angle flip = angle + (fromIntegral flip * degrees30)
        anglePlus150 :: Double -> Int -> Double
        anglePlus150 angle flip = angle + (fromIntegral flip * degrees150)        
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)
        calcP1 origin len angle = fst origin + (len * cos angle)
        calcP2 origin len angle = snd origin + (len * sin angle)       
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenf len
                angle' = fromIntegral flip * angle + degrees90
            in (calcP1 origin len' angle', calcP2 origin len' angle')
        moveOriginMinus30 :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOriginMinus30 len angle origin flip =
            let len' = lenf len
                angle' = fromIntegral flip * angle - degrees30
            in (calcP1 origin len' angle', calcP2 origin len' angle')
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) ] --1
        generator = 
            fmap ctorVecRule 
                [ (lenf, anglePlus30, project3of4, 1, 1) --1
                , (lenf, angleMinus90, moveOrigin, 1, 1) --2
                , (lenf, anglePlus150, moveOriginMinus30, 1, 1) ] --3
    in
        (initiator, generator)

islands :: ([VecRule], [VecRule])
islands = 
    let piOver2 = pi / 2.0
        piOver4 = pi / 4.0
        sqrt2 :: Double
        sqrt2 = sqrt 2.0
        lenDiv8 :: Double -> Double
        lenDiv8 len = len / 8.0
        degrees90 = piOver2
        anglePlus90 :: Double -> Int -> Double
        anglePlus90 angle flip = angle + (fromIntegral flip * degrees90)
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)
        angleMinus180 :: Double -> Int -> Double
        angleMinus180 angle flip = angle - (fromIntegral flip * pi)        
        calcP1 origin len angle = fst origin + (len * cos angle)
        calcP2 origin len angle = snd origin + (len * sin angle)
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenDiv8 len
                angle' = fromIntegral flip * angle + piOver2
            in (calcP1 origin len' angle', calcP2 origin len' angle')        
        revertOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        revertOrigin len angle origin flip =
            let len' = lenDiv8 len
                angle' = fromIntegral flip * angle - piOver2
            in (calcP1 origin len' angle', calcP2 origin len' angle')                 
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) --1
                , (id, angleMinus90, project3of4, 1, 1) --2
                , (id, angleMinus180, project3of4, 1, 1) --3
                , (id, anglePlus90, project3of4, 1, 1) ] --4
        generator = 
            fmap ctorVecRule 
                [ (lenDiv8, project1of2, project3of4, 1, 1) --1
                , (lenDiv8, anglePlus90, moveOrigin, 1, 1) --2
                , (lenDiv8, anglePlus90, project3of4, 1, 1) --3
                , (lenDiv8, project1of2, project3of4, 1, 1) --4
                , (lenDiv8, angleMinus90, project3of4, 1, 1) --5
                , (lenDiv8, angleMinus90, project3of4, 1, 1) --6
                , (lenDiv8, angleMinus180, project3of4, 1, 1) --7
                , (lenDiv8, project1of2, revertOrigin, 1, 1) --8
                , (lenDiv8, project1of2, project3of4, 1, 1) --9
                , (lenDiv8, anglePlus90, project3of4, 1, 1) --10
                , (lenDiv8, project1of2, project3of4, 1, 1) --11
                , (lenDiv8, angleMinus90, project3of4, 1, 1) --12
                , (lenDiv8, project1of2, project3of4, 1, 1) --13
                , (lenDiv8, project1of2, project3of4, 1, 1) --14
                , (lenDiv8, project1of2, project3of4, 1, 1) --15
                , (lenDiv8, project1of2, project3of4, 1, 1) ] --16
    in
        (initiator, generator)

islands2 :: ([VecRule], [VecRule])
islands2 = 
    let piOver2 = pi / 2.0
        piOver4 = pi / 4.0
        sqrt2 :: Double
        sqrt2 = sqrt 2.0
        lenDiv8 :: Double -> Double
        lenDiv8 len = len / 8.0
        degrees90 = piOver2
        degrees180 = pi
        anglePlus90 :: Double -> Int -> Double
        anglePlus90 angle flip = angle + (fromIntegral flip * degrees90)
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)
        angleMinus180 :: Double -> Int -> Double
        angleMinus180 angle flip = angle - (fromIntegral flip * degrees180)        
        calcP1 origin len angle = fst origin + (len * cos angle)
        calcP2 origin len angle = snd origin + (len * sin angle)
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenDiv8 len
                angle' = fromIntegral flip * angle + piOver2
            in (calcP1 origin len' angle', calcP2 origin len' angle')        
        revertOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        revertOrigin len angle origin flip =
            let len' = lenDiv8 len
                angle' = fromIntegral flip * angle - piOver2
            in (calcP1 origin len' angle', calcP2 origin len' angle')                 
        originScaleBy3 :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        originScaleBy3 len angle origin flip =
            let angle' = fromIntegral flip * angle
            in (calcP1 origin len angle', calcP2 origin len angle')
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) --1
                , (id, angleMinus90, project3of4, 1, 1) --2
                , (id, angleMinus180, project3of4, 1, 1) --3
                , (id, anglePlus90, project3of4, 1, 1) ] --4
        generator = 
            fmap ctorVecRule 
                [ (lenDiv8, project1of2, project3of4, 1, 1) --1
                , (lenDiv8, anglePlus90, moveOrigin, 1, 1) --2 
                , (lenDiv8, anglePlus90, project3of4, 1, 1) --3 
                , (lenDiv8, project1of2, project3of4, 1, 1) --4 
                , (lenDiv8, project1of2, project3of4, 1, 1) --5 
                , (lenDiv8, angleMinus90, project3of4, 1, 1) --6 
                , (lenDiv8, angleMinus90, project3of4, 1, 1) --7 
                , (lenDiv8, angleMinus180, project3of4, 1, 1) --8
                , (lenDiv8, angleMinus180, project3of4, 1, 1) --9
                , (lenDiv8, project1of2, revertOrigin, 1, 1) --10
                -- , (lenDiv8, project1of2, revertOrigin, 1, 1) --11 (interesting effect inadvertently discovered)
                , (lenDiv8, project1of2, project3of4, 1, 1) --11
                , (lenDiv8, project1of2, project3of4, 1, 1) --12
                , (lenDiv8, project1of2, project3of4, 1, 1) --13
                , (lenDiv8, project1of2, project3of4, 1, 1) --14
                , (lenDiv8, project1of2, project3of4, 1, 1) --15
                , (lenDiv8, project1of2, project3of4, 1, 1) ]--16
    in
        (initiator, generator)

islandsAndLakes :: ([VecRule], [VecRule])
islandsAndLakes = 
    let piOver2 = pi / 2.0
        piOver4 = pi / 4.0
        sqrt2 :: Double
        sqrt2 = sqrt 2.0
        lenDiv6 :: Double -> Double
        lenDiv6 len = len / 6.0
        degrees90 = piOver2
        anglePlus90 :: Double -> Int -> Double
        anglePlus90 angle flip = angle + (fromIntegral flip * degrees90)
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)
        angleMinus180 :: Double -> Int -> Double
        angleMinus180 angle flip = angle - (fromIntegral flip * pi)        
        calcP1 origin len angle = fst origin + (len * cos angle)
        calcP2 origin len angle = snd origin + (len * sin angle)
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenDiv6 len
                angle' = fromIntegral flip * angle + piOver2
            in (calcP1 origin len' angle', calcP2 origin len' angle')        
        revertOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        revertOrigin len angle origin flip =
            let len' = lenDiv6 len
                angle' = fromIntegral flip * angle - piOver2
            in (calcP1 origin len' angle', calcP2 origin len' angle')                 
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) --1
                , (id, angleMinus90, project3of4, 1, 1) --2            
                , (id, angleMinus180, project3of4, 1, 1) --3          
                , (id, anglePlus90, project3of4, 1, 1) ] --4         
        generator = 
            fmap ctorVecRule 
                [ (lenDiv6, project1of2, project3of4, 1, 1) --1
                , (lenDiv6, anglePlus90, moveOrigin, 1, 1) --2             
                , (lenDiv6, project1of2, project3of4, 1, 1) --3             
                , (lenDiv6, project1of2, project3of4, 1, 1) --4            
                , (lenDiv6, angleMinus90, project3of4, 1, 1) --5        
                , (lenDiv6, angleMinus180, project3of4, 1, 1) --6       
                , (lenDiv6, angleMinus180, project3of4, 1, 1) --7      
                , (lenDiv6, project1of2, revertOrigin, 1, 1) --8   
                , (lenDiv6, project1of2, project3of4, 1, 1) --9  
                , (lenDiv6, angleMinus90, revertOrigin, 1, 1) --10
                , (lenDiv6, project1of2, project3of4, 1, 1) --11
                , (lenDiv6, project1of2, project3of4, 1, 1) --12
                , (lenDiv6, anglePlus90, project3of4, 1, 1) --13
                , (lenDiv6, angleMinus180, project3of4, 1, 1) --14
                , (lenDiv6, angleMinus180, project3of4, 1, 1) --15
                , (lenDiv6, project1of2, moveOrigin, 1, 1) --16
                , (lenDiv6, project1of2, project3of4, 1, 1) --17
                , (lenDiv6, project1of2, project3of4, 1, 1) ] --18
    in
        (initiator, generator)

mandelbrotPeanoCurveIntervals13 :: ([VecRule], [VecRule])
mandelbrotPeanoCurveIntervals13 = 
    let lenDiv3 len = len / 3.0
        degrees60 = pi / 3.0
        anglePlus60 angle flip = angle + (degrees60 * fromIntegral flip)
        angleLess60 angle flip = angle - (degrees60 * fromIntegral flip)
        fivePiOver6 = 5.0 * pi / 6.0
        anglePlus5PiOver6 angle flip = angle + (fivePiOver6 * fromIntegral flip)
        angleLess5PiOver6 angle flip = angle - (fivePiOver6 * fromIntegral flip)
        lenDiv3sqrt3 len = len / (3.0 * sqrt 3.0)
        angleLessPiHalves angle flip = angle - (fromIntegral flip * pi / 2.0)
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) ] --1
        generator = 
            fmap ctorVecRule 
                [ (lenDiv3, anglePlus60, project3of4, - 1, 1) --1            
                , (lenDiv3, anglePlus60, project3of4, 1, 1) --2              
                , (lenDiv3, project1of2, project3of4, 1, 1) --3            
                , (lenDiv3, angleLess60, project3of4, 1, 1) --4          
                , (lenDiv3sqrt3, anglePlus5PiOver6, project3of4, 1, 1) --5          
                , (lenDiv3sqrt3, anglePlus5PiOver6, project3of4, - 1, 1) --6      
                , (lenDiv3sqrt3, angleLess5PiOver6, project3of4, - 1, 1) --7     
                , (lenDiv3sqrt3, angleLessPiHalves, project3of4, - 1, 1) --8  
                , (lenDiv3, project1of2, project3of4, 1, 1) --9
                , (lenDiv3sqrt3, angleLess5PiOver6, project3of4, 1, 1) --10
                , (lenDiv3sqrt3, angleLess5PiOver6, project3of4, - 1, 1) --11
                , (lenDiv3, project1of2, project3of4, - 1, 1) --12
                , (lenDiv3, project1of2, project3of4, 1, 1) ] --13
    in
        (initiator, generator)

sierpinskiCarpet :: ([VecRule], [VecRule])
sierpinskiCarpet = 
    let piOver2 = pi / 2.0
        piOver4 = pi / 4.0
        sqrt2 :: Double
        sqrt2 = sqrt 2.0
        lenDiv3 :: Double -> Double
        lenDiv3 len = len / 3.0
        degrees90 = piOver2
        anglePlus90 :: Double -> Int -> Double
        anglePlus90 angle flip = angle + (fromIntegral flip * degrees90)
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)  
        angleMinus180 :: Double -> Int -> Double
        angleMinus180 angle flip = angle - (fromIntegral flip * pi)
        calcP1 origin len angle = fst origin + (len * (cos angle))
        calcP2 origin len angle = snd origin + (len * (sin angle))
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = 2.0 * (lenDiv3 len) * sqrt2
                angle' = fromIntegral flip * angle + piOver2 + piOver4
            in (calcP1 origin len' angle', calcP2 origin len' angle')
        revertOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        revertOrigin len angle origin flip =
            let len' = 2.0 * (lenDiv3 len) * sqrt2
                angle' = fromIntegral flip * angle - piOver2 - piOver4
            in (calcP1 origin len' angle', calcP2 origin len' angle')         
        originScaleBy3 :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        originScaleBy3 len angle origin flip =
            let angle' = fromIntegral flip * angle
            in (calcP1 origin len angle', calcP2 origin len angle')
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) --1            
                , (id, angleMinus90, project3of4, 1, 1) --2               
                , (id, angleMinus180, project3of4, 1, 1) --3             
                , (id, anglePlus90, project3of4, 1, 1) ] --4             
        generator = 
            fmap ctorVecRule 
                [ (lenDiv3, project1of2, moveOrigin, 1, 1) --1            
                , (lenDiv3, angleMinus90, project3of4, 1, 1) --2             
                , (lenDiv3, angleMinus180, project3of4, 1, 1) --3           
                , (lenDiv3, anglePlus90, project3of4, 1, 1) --4          
                , (lenDiv3, project1of2, originScaleBy3, 1, 1) --5        
                , (lenDiv3, angleMinus90, project3of4, 1, 1) --6        
                , (lenDiv3, angleMinus180, project3of4, 1, 1) --7        
                , (lenDiv3, anglePlus90, project3of4, 1, 1) --8        
                , (lenDiv3, project1of2, project3of4, 1, 1) --9       
                , (lenDiv3, project1of2, revertOrigin, 1, 1) --10      
                , (lenDiv3, project1of2, project3of4, 1, 1) --11   
                , (lenDiv3, project1of2, project3of4, 1, 1) ] --12   
    in
        (initiator, generator)

sierpinskiCarpet2 :: ([VecRule], [VecRule])
sierpinskiCarpet2 = 
    let project1of2 x y = x
        project3of4 w x y z = y
        lenDiv3 :: Double -> Double
        lenDiv3 len = len / 3.0
        piOver2 = pi / 2.0
        degrees90 = piOver2
        degrees180 = pi
        anglePlus90 :: Double -> Int -> Double
        anglePlus90 angle flip = angle + (fromIntegral flip * degrees90)
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)
        anglePlus180 :: Double -> Int -> Double
        anglePlus180 angle flip = angle + (fromIntegral flip * degrees180)
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenDiv3 len
            in (fst origin + (len' * (cos angle)), snd origin + (len' * (sin angle)))           
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) ] --1
        generator = 
            fmap ctorVecRule 
                [ (lenDiv3, project1of2, project3of4, 1, 1) -- 1 
                , (lenDiv3, anglePlus90, project3of4, 1, 1) --2
                , (lenDiv3, project1of2, project3of4, 1, 1) --3
                , (lenDiv3, angleMinus90, project3of4, 1, 1) --4
                , (lenDiv3, angleMinus90, project3of4, 1, 1) --5
                , (lenDiv3, anglePlus180, project3of4, 1, 1) --6
                , (lenDiv3, anglePlus90, project3of4, 1, 1) --7
                , (lenDiv3, project1of2, moveOrigin, 1, 1) ] --8
    in (initiator, generator)            

sierpinskiGasket2 :: ([VecRule], [VecRule])
sierpinskiGasket2 = 
    let lenDiv2 :: Double -> Double
        lenDiv2 len = len / 2.0
        degrees60 = pi / 3.0
        degrees120 = 2 * degrees60
        angleMinus120 :: Double -> Int -> Double
        angleMinus120 angle flip = angle - (fromIntegral flip * degrees120)
        anglePlus120 :: Double -> Int -> Double
        anglePlus120 angle flip = angle + (fromIntegral flip * degrees120)
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenDiv2 len
                angle' = fromIntegral flip * angle - degrees60
            in (fst origin + (len' * (cos angle')), snd origin + (len' * (sin angle')))           
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) --1
                , (id, anglePlus120, project3of4, 1, 1) --2
                , (id, angleMinus120, project3of4, 1, 1) ] --3
        generator = 
            fmap ctorVecRule 
                [ (lenDiv2, project1of2, project3of4, 1, 1) -- 1 
                , (lenDiv2, anglePlus120, project3of4, 1, 1) --2
                , (lenDiv2, project1of2, moveOrigin, 1, 1) ] --3
    in (initiator, generator)

tiles :: ([VecRule], [VecRule])
tiles = 
    let lenDiv3 :: Double -> Double
        lenDiv3 len = len / 3.0
        degrees90 = pi / 2.0
        degrees180 = pi
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)
        angleMinus180 :: Double -> Int -> Double
        angleMinus180 angle flip = angle - (fromIntegral flip * pi)        
        anglePlus180 :: Double -> Int -> Double
        anglePlus180 angle flip = angle + (fromIntegral flip * degrees180)
        anglePlus90 :: Double -> Int -> Double
        anglePlus90 angle flip = angle + (fromIntegral flip * degrees90)        
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenDiv3 len
                angle' = fromIntegral flip * angle - degrees90
            in (fst origin + (len' * (cos angle')), snd origin + (len' * (sin angle')))           
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) --1
                , (id, angleMinus90, project3of4, 1, 1) --2
                , (id, anglePlus180, project3of4, 1, 1) --3
                , (id, anglePlus90, project3of4, 1, 1) ] --4
        generator = 
            fmap ctorVecRule 
                [ (lenDiv3, project1of2, moveOrigin, 1, 1) -- 1 
                , (lenDiv3, project1of2, project3of4, 1, 1) --2
                , (lenDiv3, angleMinus90, project3of4, 1, 1) --3
                , (lenDiv3, project1of2, project3of4, 1, 1) --4
                , (lenDiv3, angleMinus90, project3of4, 1, 1) --5
                , (lenDiv3, anglePlus180, project3of4, 1, 1) --6
                , (lenDiv3, anglePlus180, project3of4, 1, 1) ] --7
    in (initiator, generator)

tiles2 :: ([VecRule], [VecRule])
tiles2 = 
    let lenDiv3 :: Double -> Double
        lenDiv3 len = len / 3.0
        degrees90 = pi / 2.0
        degrees180 = pi
        anglePlus90 :: Double -> Int -> Double
        anglePlus90 angle flip = angle + (fromIntegral flip * degrees90)        
        angleMinus90 :: Double -> Int -> Double
        angleMinus90 angle flip = angle - (fromIntegral flip * degrees90)
        angleMinus180 :: Double -> Int -> Double
        angleMinus180 angle flip = angle - (fromIntegral flip * pi)        
        anglePlus180 :: Double -> Int -> Double
        anglePlus180 angle flip = angle + (fromIntegral flip * degrees180)
        moveOrigin :: Double -> Double -> (Double, Double) -> Int -> (Double, Double)
        moveOrigin len angle origin flip =
            let len' = lenDiv3 len
            in (fst origin + (len' * cos angle), snd origin + (len' * sin angle))           
        initiator = 
            fmap ctorVecRule 
                [ (id, project1of2, project3of4, 1, 1) --1
                , (id, anglePlus90, project3of4, 1, 1) --2
                , (id, anglePlus180, project3of4, 1, 1) --3
                , (id, angleMinus90, project3of4, 1, 1) ] --4
        generator = 
            fmap ctorVecRule 
                [ (lenDiv3, project1of2, moveOrigin, 1, 1) -- 1 
                , (lenDiv3, project1of2, project3of4, 1, 1) --2
                , (lenDiv3, anglePlus90, project3of4, 1, 1) --3
                , (lenDiv3, anglePlus180, project3of4, 1, 1) --4
                , (lenDiv3, anglePlus90, project3of4, 1, 1) --5
                , (lenDiv3, anglePlus180, project3of4, 1, 1) --6
                , (lenDiv3, anglePlus180, project3of4, 1, 1) ] --7
    in (initiator, generator)           

vectorEndPoint :: UI.Point -> Double -> Double -> UI.Point
vectorEndPoint vec len angle =
    (fst vec + (len * cos angle), snd vec + (len * sin angle))

scmToInt :: ScmObject -> Maybe Int
scmToInt (ObjImmediate (ImmInt x)) = Just x
scmToInt _ = Nothing

isScmClosure :: ScmObject -> Maybe ScmObject
isScmClosure x@(ObjClosure _) = Just x
isScmClosure _ = Nothing

lispToVecRule :: ScmObject -> Maybe VecRule
lispToVecRule x@(ObjCons _) = 
    case cnsToList x of
        Just l -> 
            let flipa = scmToInt $ l !! 3
                flipr = scmToInt $ l !! 4
                lenf = isScmClosure $ head l
                anglef = isScmClosure $ l !! 1
                originf = isScmClosure $ l !! 2
            in 
                case (lenf, anglef, originf, flipa, flipr) of
                    (Just l, Just a, Just o, Just fa, Just fr) -> 
                        Just $ VecRule 
                            { vrlLenf = VlnScheme l
                            , vrlAnglef = VanScheme a
                            , vrlOriginf = VorScheme o
                            , vrlFlipAngle = fa
                            , vrlFlipRules = fr }
                    _ -> Nothing
        _ -> Nothing
lispToVecRule _ = Nothing 

rulesToList :: Maybe ScmObject -> Maybe [VecRule]
rulesToList (Just x@(ObjCons _)) = 
    case cnsToList x of
        Just t -> 
            Just $ mapMaybe lispToVecRule t
        _ -> 
            Nothing
rulesToList _ = Nothing

fetchRules :: String -> Bool -> ([VecRule], [VecRule])
fetchRules rules builtIn =
    if builtIn then
        case findBuiltin builtIns rules of
            Just x -> x
            Nothing -> error $ "couldn't find built-in rule for " ++ rules --to do:  remove this
    else --call LISP
        let evaled = evalString rules
            evaledStr = 
                case evaled of
                    Right x -> 
                        let exp = Just $ last x
                            car = rulesToList $ safeCar exp
                            cdr = rulesToList $ safeCar $ safeCdr exp
                        in case (car, cdr) of
                            (Just seed, Just rules) -> (seed, rules)
                            _ -> error "couldn't make seed and rules from LISP"
                    Left x -> error $ "fetchRules:  scheme eval failed.  Error:  " ++ show x --to do:  remove error
        in evaledStr

{-
There are two coloring cases: 
    1) the same diagram repeats at different sizes
    2) all diagrams are the same size

For coloring type #1, look for the sizes of diagrams and color based on size.
For type #2, decide how many groups you want and color accordingly
-}

drawFunction :: [Vector] -> VectorColorizer -> Double -> Double -> UI.Point -> (UI.Point, [Vector], VectorColorizer)
drawFunction vecs clz len angle origin =
    let (x, y) = origin
        pt2 = (x + len * cos angle, y + len * sin angle)
        ln = (origin, pt2)
        idx = length vecs
        -- chk p i j n = i >= n * p && i < (n + j) * p --probably experimental code artifact
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

quitFunction :: Int -> Double -> Int -> Bool --in common
quitFunction generations = 
    if generations < 0 then --perhaps this is to stop recursing based on length of the vector rather than generations; shouldn't use neg numbers like this (use sum type instead)
        \len generation -> len < fromIntegral generations
    else 
        \len generation -> generation == generations     

vectorFractal :: XmlObj -> Either [ScmError] [Vector]
vectorFractal xob@XmlObj 
    { xobName = n
    , xobDesc = d
    , xobGenerations = gen
    , xobBuiltIn = True
    , xobContinuous = continuous
    , xobLength = l
    , xobRules = r
    , xobColors = colors
    , xobAlgorithm = colorAlg } =
    let (seed, rules) = fetchRules r True
        pp = ctorPalettePicker colors
        numRules = length rules
        colorizer =
            case colorAlg of
                CalLevel lvl -> 
                    VczLevel (ctorLevelColorizer pp lvl gen)
                CalImage subtractor -> 
                    VczImage (ctorSizeColorizer pp numRules subtractor gen)
        twoDvectorFractal ::
            [VecRule] -> --seed
            [VecRule] -> --rules
            Double -> --len
            Double -> --angle
            Double -> --xorigin
            Double -> --yorigin
            (Double -> Int -> Bool) -> --quitf --to do:  determine if this needs to be passed as an argument
            (UI.Point, [Vector], VectorColorizer) --origin (return value)
        twoDvectorFractal
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
                across _ _ origin _ _ _ [] vectors colorizer = --rules are empty
                    (origin, vectors, colorizer) -- to do:  move origin to right before vectors
                across --across for builtin
                    len 
                    angle 
                    origin 
                    flipAngleFactor 
                    flipRulesFactor 
                    generation 
                    (VecRule 
                        { vrlLenf = VlnBuiltIn l
                        , vrlAnglef = VanBuiltIn a
                        , vrlOriginf = VorBuiltIn o
                        , vrlFlipAngle = flipAngle
                        , vrlFlipRules = flipRules } : restSeed) 
                    vectors 
                    colorizer 
                    = --across for builtin
                    let lenf = l len
                        anglef = a angle flipAngleFactor
                        (newOrigin, vectors', colorizer') =
                            across len angle origin flipAngleFactor flipRulesFactor generation restSeed vectors colorizer
                        originf = o len angle newOrigin flipAngleFactor
                    in
                        down lenf anglef originf (flipAngleFactor * flipAngle) (flipRulesFactor * flipRules) (generation + 1) vectors' colorizer'
                across _ _ _ _ _ _ _ _ _ = error "unexpected arguments to across"
                down :: Double -> Double -> UI.Point -> Int -> Int -> Int -> [Vector] -> VectorColorizer -> (UI.Point, [Vector], VectorColorizer)
                down len angle origin flipAngleFactor flipRulesFactor generation vectors colorizer =
                    let colorizer' =
                            case colorizer of
                                VczLevel l ->
                                    VczLevel $ checkLevel l generation
                                _ -> 
                                    colorizer
                    in 
                        let quit = quitf len generation
                        in
                            if quit then
                                drawFunction vectors colorizer' len angle origin
                            else
                                let rules' = 
                                        if flipRulesFactor > 0 then
                                            reverse rules
                                        else
                                            rules
                                    newOrigin@(_, vecs, clz) =
                                        across len angle origin flipAngleFactor flipRulesFactor generation rules' vectors colorizer'
                                in 
                                    if continuous then
                                        newOrigin
                                    else 
                                        (vectorEndPoint origin len angle, vecs, clz)
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
    in
        let (_, vectors, _) = 
                twoDvectorFractal 
                    seed --initiator
                    rules --generator
                    l --len
                    0.0 --start angle
                    0.0 --flip angle
                    0.0 --flip rules
                    (quitFunction gen)
        in Right vectors
vectorFractal xob@XmlObj --lisp specified vector fractals
    { xobName = _
    , xobDesc = _
    , xobGenerations = gen
    , xobBuiltIn = False
    , xobContinuous = continuous
    , xobLength = l
    , xobRules = r
    , xobColors = colors
    , xobAlgorithm = colorAlg } 
    =
    let (seed, rules) = fetchRules r False
        pp = ctorPalettePicker colors
        numRules = length rules
        colorizer =
            case colorAlg of
                CalLevel lvl -> 
                    VczLevel (ctorLevelColorizer pp lvl gen)
                CalImage subtractor -> 
                    VczImage (ctorSizeColorizer pp numRules subtractor gen)
        twoDvectorFractal :: --to do:  determine if drawf needs to be passed:  it's visible via scope
            -- ([Vector] -> VectorColorizer -> Double -> Double -> (Double, Double) -> ((Double, Double), [Vector], VectorColorizer)) -> --drawf
            [VecRule] -> --seed
            [VecRule] -> --rules
            Double -> --len
            Double -> --angle
            Double -> --xorigin
            Double -> --yorigin
            (Double -> Int -> Bool) -> --quitf --to do:  determine if this needs to be passed as an argument
            Either [ScmError] (UI.Point, [Vector], VectorColorizer) --origin (return value)
        twoDvectorFractal
            seed
            rules
            len
            angle
            xOrigin
            yOrigin
            quitf
            =  
            let across :: Double -> Double -> UI.Point -> Int -> Int -> Int -> [VecRule] -> [Vector] -> VectorColorizer -> Either [ScmError] (UI.Point, [Vector], VectorColorizer)
                across _ _ origin _ _ _ [] vectors colorizer = 
                    Right (origin, vectors, colorizer) -- to do:  move origin to right before vectors
                across --across function for lisp
                    len 
                    angle 
                    origin 
                    flipAngleFactor 
                    flipRulesFactor 
                    generation 
                    (VecRule 
                        { vrlLenf = VlnScheme l
                        , vrlAnglef = VanScheme a
                        , vrlOriginf = VorScheme o
                        , vrlFlipAngle = flipAngle
                        , vrlFlipRules = flipRules } : restSeed) 
                    vectors 
                    colorizer 
                    = --across for lisp
                    case across len angle origin flipAngleFactor flipRulesFactor generation restSeed vectors colorizer of
                        Right (newOrigin, vectors', colorizer') -> --across is okay, we can attempt down call
                            let scmLen = toScheme $ SopDouble len
                                lenf = scmApply l [scmLen]
                                scmAngle = toScheme $ SopDouble angle
                                scmFlipFactor = toScheme $ SopInt flipAngleFactor
                                anglef = scmApply a [scmAngle, scmFlipFactor]
                                scmNewOrigin = toScheme $ SopTuple newOrigin
                                originf = scmApply o [scmLen, scmAngle, scmNewOrigin, scmFlipFactor]
                                scmRights = mapMaybe fmScheme (rights [lenf, anglef, originf])
                                scmLefts = concat $ lefts [lenf, anglef, originf]
                            in
                                if not $ null scmLefts then
                                    Left scmLefts --to do:  add another error for across, cons in
                                else
                                    case scmRights of 
                                        [SopDouble len', SopDouble angle', SopTuple origin'] ->
                                            down len' angle' origin' (flipAngleFactor * flipAngle) (flipRulesFactor * flipRules) (generation + 1) vectors' colorizer' 
                                        _ -> 
                                            Left [ ScmError { errMessage = "scheme calls for across did not all succeed", errCaller = "across" } ]
                        e@(Left _) -> --we're toast already
                            e
                across --across function for lisp
                    len 
                    angle 
                    origin 
                    flipAngleFactor 
                    flipRulesFactor 
                    generation 
                    (currentSeed : restSeed) 
                    vectors 
                    colorizer 
                    = --across for lisp, last pattern
                    Left [ ScmError { errMessage = "this pattern should never trigger", errCaller = "across" }]
                down :: Double -> Double -> UI.Point -> Int -> Int -> Int -> [Vector] -> VectorColorizer -> Either [ScmError] (UI.Point, [Vector], VectorColorizer)
                down len angle origin flipAngleFactor flipRulesFactor generation vectors colorizer =
                    let colorizer' =
                            case colorizer of
                                VczLevel l ->
                                    VczLevel $ checkLevel l generation
                                _ -> 
                                    colorizer
                    in 
                        let quit = quitf len generation
                        in
                            if quit then
                                Right $ drawFunction vectors colorizer' len angle origin
                            else
                                let rules' = 
                                        if flipRulesFactor > 0 then
                                            reverse rules
                                        else
                                            rules
                                    newOrigin = --(origin, vectors, colorizer')
                                        across len angle origin flipAngleFactor flipRulesFactor generation rules' vectors colorizer'
                                in 
                                    if continuous then
                                        newOrigin
                                    else 
                                        case newOrigin of 
                                            Right (_, vecs, clz) -> 
                                                Right (vectorEndPoint origin len angle, vecs, clz)
                                            Left x -> Left x
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
    in 
        let res = 
                twoDvectorFractal 
                    seed --initiator
                    rules --generator
                    l --len
                    0.0 --start angle
                    0.0 --flip angle
                    0.0 --flip rules
                    (quitFunction gen)
        in case res of 
            Right (_, vectors, _) -> Right vectors
            Left e -> Left e                  