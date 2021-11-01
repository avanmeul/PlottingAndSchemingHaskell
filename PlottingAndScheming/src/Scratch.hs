module Scratch where

import Control.Monad.State
import Data.Function
import Data.Word
import Data.List
import Data.Maybe

strm = [1,2,3,3,2,1,5,7,9,12,3,3,8,9]

highestDelta lst = 
    iter lst 0 Nothing where
        iter :: [Int] -> Int -> Maybe Int -> Int
        iter lst highestDelta lowestVal =
            let lowest = fromMaybe (head lst) lowestVal 
            in
                case lst of 
                    [] -> highestDelta
                    (h : t) -> 
                        if h - lowest > highestDelta 
                        then iter t (h - lowest) (Just lowest)
                        else iter t highestDelta (Just lowest)

scratchResult = highestDelta strm

questions = 
    [ (1, "Nouns", 1, "Common Nouns", 1, 0.7)
    , (1, "Nouns", 1, "Common Nouns", 2, 0.6)
    , (1, "Nouns", 2, "Abstract Nouns", 3, 0.8)
    , (1, "Nouns", 3, "Proper Nouns", 4, 0.2)    
    , (1, "Nouns", 3, "Proper Nouns", 5, 0.5)    
    , (1, "Nouns", 3, "Proper Nouns", 6, 0.4)
    , (2, "Verbs", 4, "Action Verbs", 7, 0.9)    
    , (2, "Verbs", 4, "Action Verbs", 8, 0.1)    
    , (2, "Verbs", 5, "Transitive Verbs", 9, 0.3)    
    , (2, "Verbs", 5, "Transitive Verbs", 10, 0.6)
    , (2, "Verbs", 5, "Transitive Verbs", 11, 0.4)    
    , (2, "Verbs", 6, "Reflexive Verbs", 12, 0.2) ]

--39 minutes into the test I have a panel of questions as follows (here I'm using 5 as my n, but n can be anything; this assures an equal distribution of questions used)

panel = take 5 $ cycle questions

sortGT (_, _, _, _, _, d1) (_, _, _, _, _, d2) =
    if d1 < d2 then GT
    else if d1 > d2 then LT
    else EQ
sortedPanel = sortBy sortGT panel --we want a GT sort for difficulty, so we need to reverse or flip

--now:  start enforcing the different requirements

{-
split the questions on strand (Nouns, Verbs) using a split function in Data.List.Split, or a filter function
(tricky part!) split the questions in the split lists into separate lists based on standard id; 
    then walk across them consuming the head of each sublist, and collecting a list of the tails (of each sublist) for subsequent recursive calls
    as each sublist runs out, it is removed from the list of (sub)lists argument to the recursive call
interleave the resulting noun and verb lists
take n from the cycle of the interleaved lists
sort the panel of questions from easiest to hardest using sortBy with the predicate checking the last value of the tuple
-}

nouns = filter (\ (n, _, _, _, _, _) -> n == 1)
verbs = filter (\ (n, _, _, _, _, _) -> n == 2)


code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop
    io $ print y
    return ()

--
-- pop the next unique off the stack
--
pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO

-- result :: String
result :: IO ()
result = runStateT code [1..] >> return ()

--code from Learn You a Haskell

-- type Stack = [Int]

-- pop :: State Stack Int
-- pop = state $ \(x:xs) -> (x, xs)

-- push :: Int -> State Stack ()
-- push a = state $ \xs -> ((), a:xs)

-- stackManip :: State Stack Int
-- stackManip = do
--     push 3
--     pop
--     pop

-- result :: String
-- result = show $ runState stackManip [5,8,2,1]