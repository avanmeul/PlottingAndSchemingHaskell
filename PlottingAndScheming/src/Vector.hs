module Vector where

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

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

--to do:  move these to Main.hs?

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
                        y1' = yh - y1 --quadrant I
                        -- y1' = y1 - yl
                        x2' = x2 - xl
                        -- y2' = y2 - yl
                        y2' = yh - y2 --quadrant I
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
        desc1 = info1 >>= C.element desc >>= child >>= content
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
    
data VecAngle =
    VanBuiltIn (Double -> Int -> Double) |
    VanScheme ScmObject

data VecOrigin =
    VorBuiltIn (Double -> Double -> UI.Point -> Int -> UI.Point) |
    VorScheme ScmObject
   
-- data VecFlip = --to do:  orphan and remove this
--     FlpBuiltIn Int |
--     FlpScheme ScmObject --must return an int

data VecRule = VecRule 
    { vrlLenf :: VecLen
    , vrlAnglef :: VecAngle
    , vrlOriginf :: VecOrigin
    , vrlFlipAngle :: Int
    , vrlFlipRules :: Int
    }

mandelbrotPeanoCurveIntervals13 :: ([VecRule], [VecRule])
mandelbrotPeanoCurveIntervals13 = 
    let project1of2 x y = x
        project3of4 w x y z = y
        lenDiv3 len = len / 3.0
        degrees60 = pi / 3.0
        anglePlus60 angle flip = angle + (degrees60 * (fromIntegral flip))
        angleLess60 angle flip = angle - (degrees60 * (fromIntegral flip))
        fivePiOver6 = 5.0 * pi / 6.0
        anglePlus5PiOver6 angle flip = angle + (fivePiOver6 * (fromIntegral flip))
        angleLess5PiOver6 angle flip = angle - (fivePiOver6 * (fromIntegral flip))
        lenDiv3sqrt3 len = len / (3.0 * sqrt 3.0)
        angleLessPiHalves angle flip = angle - ((fromIntegral flip) * pi / 2.0)
        initiator = 
            [ VecRule 
                { vrlLenf = VlnBuiltIn id
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
                } ]
        generator = 
            [ VecRule --rule 1 (list len-div-3 angle-plus-60 project3.4 -1 1) ; 1
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn anglePlus60
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = (-1)
                , vrlFlipRules = 1
                }
            , VecRule --rule 2 (list len-div-3 angle-plus-60 project3.4 1 1) ; 2
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn anglePlus60
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
                }
            , VecRule --rule 3 (list len-div-3 project1.2 project3.4 1 1) ; 3
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
                }
            , VecRule --rule 4 (list len-div-3 angle-less-60 project3.4 1 1) ; 4
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn angleLess60
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
                }
            , VecRule --rule 5 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 1 1) ; 5
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn anglePlus5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
                }
            , VecRule --rule 6 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 -1 1) ; 6
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn anglePlus5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = (-1)
                , vrlFlipRules = 1
                }
            , VecRule --rule 7 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 7
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLess5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = (-1)
                , vrlFlipRules = 1
                }                              
            , VecRule --rule 8 (list len-div-3sqrt3 angle-less-pi-halves project3.4 -1 1) ; 8
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLessPiHalves
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = (-1)
                , vrlFlipRules = 1
                }  
            , VecRule --rule 9 (list len-div-3 project1.2 project3.4 1 1) ; 9
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
                }  
            , VecRule --rule 10 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 1 1) ; 10
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLess5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
                }  
            , VecRule --rule 11 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 11
                { vrlLenf = VlnBuiltIn lenDiv3sqrt3
                , vrlAnglef = VanBuiltIn angleLess5PiOver6
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = (-1)
                , vrlFlipRules = 1
                }  
            , VecRule --rule 12 (list len-div-3 project1.2 project3.4 -1 1) ; 12
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = (-1)
                , vrlFlipRules = 1
                }  
            , VecRule --rule 13 (list len-div-3 project1.2 project3.4 1 1)) ; 13
                { vrlLenf = VlnBuiltIn lenDiv3
                , vrlAnglef = VanBuiltIn project1of2
                , vrlOriginf = VorBuiltIn project3of4
                , vrlFlipAngle = 1
                , vrlFlipRules = 1
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

-- selectBuiltin :: String -> Maybe ([VecRule], [VecRule])
-- selectBuiltin nm = 
--     if nm == "mandelbrotPeanoCurveIntervals13" then 
--         Just mandelbrotPeanoCurveIntervals13 
--     else 
--         Nothing

vectorEndPoint :: UI.Point -> Double -> Double -> UI.Point
vectorEndPoint vec len angle =
    (((fst vec) + (len * cos angle)), ((snd vec) + (len * sin angle)))

builtIns :: [ (String, ([VecRule], [VecRule]))]
builtIns = 
    [ ("mandelbrotPeanoCurveIntervals13", mandelbrotPeanoCurveIntervals13) ]

--to do:  iter with index number, determine expected types based on index number, populate VecRule, cons onto accumulator (which will be reversed when completed)

--to do:  maybe just convert to [ScmObject] via cnsToList

parseLispRule :: ScmObject -> VecRule --should return only one rule, not a list
parseLispRule x = iter x 1 [] [] where
    iter:: ScmObject -> Int -> [ScmObject] -> [Int] -> VecRule
    iter (ObjCons ScmCons { scmCar = h, scmCdr = ObjImmediate (ImmSym "()") }) n acc ints = 
        if n /= 5 then
            error "wrong number of items in LISP rule, should be 5"
        else
            --add car to acc, make sure it's an int, then reverse acc and pack into VecRule
            case h of
                ObjImmediate (ImmInt n) -> 
                    undefined
                otherwise -> error "arg 6 of a LISP rule should be an int"
    iter (ObjCons ScmCons { scmCar = h, scmCdr = t }) n acc ints= 
        --if index is 1 .. 3 car must be closure, else int
        undefined
    iter _ _ _ _ = error "bad LISP rule"

scmToInt :: ScmObject -> Maybe Int
scmToInt (ObjImmediate (ImmInt x)) = Just x
scmToInt _ = Nothing

isScmClosure :: ScmObject -> Maybe ScmObject
isScmClosure x@(ObjClosure _) = Just x
isScmClosure _ = Nothing

lispToVecRule :: ScmObject -> Maybe VecRule
lispToVecRule x@(ObjCons _) = 
    case (cnsToList x) of
        Just l -> 
            let flipa = scmToInt $ l !! 3
                flipr = scmToInt $ l !! 4
                lenf = isScmClosure $ l !! 0
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
                    otherwise -> Nothing
        otherwise -> Nothing
lispToVecRule _ = Nothing 

--map lispToVecRule over the rulesToList results, check length, do catMaybes on it, check length = 5

rulesToList :: Maybe ScmObject -> Maybe [VecRule]
rulesToList (Just x@(ObjCons _)) = 
    case (cnsToList x) of
        Just t -> 
            Just $ catMaybes $ fmap lispToVecRule t
        otherwise -> 
            Nothing
rulesToList _ = Nothing

fetchRules :: String -> Bool -> ([VecRule], [VecRule])
fetchRules rules builtIn =
    if builtIn then
        --to do search for this in builtIns
        mandelbrotPeanoCurveIntervals13 
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
                            otherwise -> error "couldn't make seed and rules from LISP"
                    Left x -> error $ "scheme eval failed:  " ++ (show x)
        in evaledStr
        
vectorFractal :: XmlObj -> [Vector]
vectorFractal xob@(XmlObj 
    { xobName = _
    , xobDesc = _
    , xobGenerations = gen
    , xobBuiltIn = True
    , xobContinuous = c
    , xobLength = l
    , xobRules = r
    , xobColors = colors
    , xobAlgorithm = colorAlg }) =
    let (seed, rules) = fetchRules r True
        pp = ctorPalettePicker colors
        numRules = length rules
        colorizer =
            case colorAlg of
                CalLevel lvl -> 
                    VczLevel (ctorLevelColorizer pp lvl gen)
                CalImage subtractor -> 
                    VczImage (ctorSizeColorizer pp numRules subtractor gen)
        {-
        There are two coloring cases: 
            1) the same diagram repeats at different sizes
            2) all diagrams are the same size

        For coloring type #1, look for the sizes of diagrams and color based on size.
        For type #2, decide how many groups you want and color accordingly
        -}
        drawFunction :: [Vector] -> VectorColorizer -> Double -> Double -> UI.Point -> (UI.Point, [Vector], VectorColorizer) --in common 
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
        twoDvectorFractal ::
            -- ([Vector] -> VectorColorizer -> Double -> Double -> (Double, Double) -> ((Double, Double), [Vector], VectorColorizer)) -> --drawf
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
                across _ _ origin _ _ _ [] vectors colorizer = (origin, vectors, colorizer) -- to do:  move origin to right before vectors
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
                    in --(newOrigin, vectors', colorizer')
                        down lenf anglef originf (flipAngleFactor * flipAngle) (flipRulesFactor * flipRules) (generation + 1) vectors' colorizer'       
                down :: Double -> Double -> UI.Point -> Int -> Int -> Int -> [Vector] -> VectorColorizer -> (UI.Point, [Vector], VectorColorizer)
                down len angle origin flipAngleFactor flipRulesFactor generation vectors colorizer =
                    let colorizer' =
                            case colorizer of
                                VczLevel l ->
                                    VczLevel $ checkLevel l generation
                                otherwise -> 
                                    colorizer
                    in 
                        let quit = quitf len generation
                        in --error $ "quit = " ++ (show quit)
                            if quit then --to do vecs clz len angle origin
                                drawFunction vectors colorizer' len angle origin
                            else
                                let rules' = 
                                        if flipRulesFactor > 0 then
                                            reverse rules
                                        else
                                            rules
                                    newOrigin = --(origin, vectors, colorizer')
                                        across len angle origin flipAngleFactor flipRulesFactor generation rules' vectors colorizer'
                                in 
                                    if xobContinuous xob then
                                        newOrigin
                                    else 
                                        (vectorEndPoint origin len angle, vectors, colorizer')
            in --((xOrigin, yOrigin), [], colorizer)
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
        quitFunction :: Int -> Double -> Int -> Bool --in common
        quitFunction generations = 
            if generations < 0 then --perhaps this is to stop recursing based on length of the vector rather than generations; shouldn't use neg numbers like this (use sum type instead)
                \len generation -> len < (fromIntegral generations)
            else 
                \len generation -> generation == generations                
    in
        let len = xobLength xob
        in --[ Vector { vecP1 = (0,0), vecP2 = (100, 100), vecColor = "blue" } ]
            let (_, vectors, _) = 
                    twoDvectorFractal 
                        seed --initiator
                        rules --generator
                        len --len
                        0.0 --start angle
                        0.0 --flip angle
                        0.0 --flip rules
                        (quitFunction gen)
            in vectors
vectorFractal xob@(XmlObj --lisp specified vector fractals
    { xobName = _
    , xobDesc = _
    , xobGenerations = gen
    , xobBuiltIn = False
    , xobContinuous = c
    , xobLength = l
    , xobRules = r
    , xobColors = colors
    , xobAlgorithm = colorAlg }) =
    let (seed, rules) = fetchRules r True
        pp = ctorPalettePicker colors
        numRules = length rules
        colorizer =
            case colorAlg of
                CalLevel lvl -> 
                    VczLevel (ctorLevelColorizer pp lvl gen)
                CalImage subtractor -> 
                    VczImage (ctorSizeColorizer pp numRules subtractor gen)
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
        twoDvectorFractal :: --to do:  determine if drawf needs to be passed:  it's visible via scope
            -- ([Vector] -> VectorColorizer -> Double -> Double -> (Double, Double) -> ((Double, Double), [Vector], VectorColorizer)) -> --drawf
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
            {-
                let lenf =
                    match currentSeed.lenf with
                    | vecLen.BuiltIn f -> 
                        (f len)
                    | vecLen.Scheme g -> 
                        let res = apply g ((toScheme len) :: [])
                        let res = fromScheme res
                        let res =
                            match res with
                            | :? int as i -> failwith "wanted a double"
                            | :? double as d -> d
                            | _ -> failwith "bad result from Scheme"
                        res
                let anglef = 
                    match currentSeed.anglef with
                    | vecAngle.BuiltIn f -> 
                        (f angle flipAngleFactor)
                    | vecAngle.Scheme s -> 
                        let args = (toScheme angle) :: (toScheme flipAngleFactor) :: []
                        let res = apply s args
                        let res = fromScheme res
                        let res =
                            match res with
                            | :? double as d -> d
                            | :? int as i -> 
                                (double i)
                            | _ -> failwith "bad result from Scheme"
                        res
                let originf =                    
                    let newOrigin =                             
                        across 
                            len 
                            angle 
                            origin 
                            flipAngleFactor
                            flipRulesFactor
                            generation 
                            restSeed
                    match currentSeed.originf with
                    | vecOrigin.BuiltIn f -> 
                        (f                       
                            len 
                            angle
                            newOrigin
                            flipAngleFactor)
                    | vecOrigin.Scheme g -> 
                        let args = 
                            (toScheme len) ::
                            (toScheme angle) ::
                            (toScheme newOrigin) :: 
                            (toScheme flipAngleFactor) :: 
                            []
                        let res = apply g args
                        let res = fromScheme res
                        let res =
                            match res with
                            | :? int as i -> failwith "wanted a double * double"
                            | :? double as d -> failwith "wanted a double * double"
                            | :? (double * double) as t -> t
                            | _ -> failwith "bad result from Scheme"
                        res

            -}
            let across :: Double -> Double -> UI.Point -> Int -> Int -> Int -> [VecRule] -> [Vector] -> VectorColorizer -> (UI.Point, [Vector], VectorColorizer)
                across _ _ origin _ _ _ [] vectors colorizer = (origin, vectors, colorizer) -- to do:  move origin to right before vectors
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
                    --to do
                    error "across for lisp code not implemented yet" --to do phase 2         
                down :: Double -> Double -> UI.Point -> Int -> Int -> Int -> [Vector] -> VectorColorizer -> (UI.Point, [Vector], VectorColorizer)
                down len angle origin flipAngleFactor flipRulesFactor generation vectors colorizer =
                    let colorizer' =
                            case colorizer of
                                VczLevel l ->
                                    VczLevel $ checkLevel l generation
                                otherwise -> 
                                    colorizer
                    in 
                        let quit = quitf len generation
                        in --error $ "quit = " ++ (show quit)
                            if quit then --to do vecs clz len angle origin
                                drawFunction vectors colorizer' len angle origin
                            else
                                let rules' = 
                                        if flipRulesFactor > 0 then
                                            reverse rules
                                        else
                                            rules
                                    newOrigin = --(origin, vectors, colorizer')
                                        across len angle origin flipAngleFactor flipRulesFactor generation rules' vectors colorizer'
                                in 
                                    if xobContinuous xob then
                                        newOrigin
                                    else 
                                        (vectorEndPoint origin len angle, vectors, colorizer')
            in --((xOrigin, yOrigin), [], colorizer)
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
        quitFunction :: Int -> Double -> Int -> Bool
        quitFunction generations = 
            if generations < 0 then --perhaps this is to stop recursing based on length of the vector rather than generations; shouldn't use neg numbers like this (use sum type instead)
                \len generation -> len < (fromIntegral generations)
            else 
                \len generation -> generation == generations                
    in --undefined
        let len = xobLength xob
        in --[ Vector { vecP1 = (0,0), vecP2 = (100, 100), vecColor = "blue" } ]
            let (_, vectors, _) = 
                    twoDvectorFractal 
                        seed --initiator
                        rules --generator
                        len --len
                        0.0 --start angle
                        0.0 --flip angle
                        0.0 --flip rules
                        (quitFunction gen)
            in vectors                      