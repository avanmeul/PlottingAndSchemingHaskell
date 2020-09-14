module Scratch where

import Control.Monad.State 

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