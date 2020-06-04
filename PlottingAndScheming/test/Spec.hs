{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
-- import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import System.IO
import GHC.Generics

-- import qualified Data.Text as T

{-
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
-}

{-
https://tech.fpcomplete.com/haskell/library/aeson
to do:  add json file
read tests from it, 
run tests
switch from String to byteString, or Text (for unicode)
-}

data Person = Person { name :: Text, age :: Int } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person

t1 = parse "(car '(234324fsdfds-sdfdsfsdf3.5))"
t1' = tokParse "(car '(234324fsdfds-sdfdsfsdf3.5))"
t2 = parse "(car (cdr (cons '(-3. . +5) (cons 4.5 23423432))))"
t2' = tokParse "(car (cdr (cons '(-3. . +5) (cons 4.5 23423432))))"
t3 = "(car '(\"hey\"))"
t3' = tokParse t3
t4 = takeFromWhile "thishey\"dsdds" (/= '"')
t5 = parseString "\"thishey\"dsdds"
-- t6 = encode [1,2,3]
-- t6 = encode $ [1,2,3] :: [Int]
-- t6 = foo "abcde"

t6 = decode "true" :: Maybe Bool
t7 = encode True

bString :: B.ByteString
bString = "This is a byte string"

tString :: T.Text
tString = "This is a text string"

strToByteStr :: B.ByteString
strToByteStr = B.pack "Converted from string to byte string"

strToText :: T.Text
strToText = T.pack "Converted from string to text"

byteToStr :: String
byteToStr = B.unpack "from byte string to string"

txtToStr :: String
txtToStr = T.unpack "from text to string"

bstToText :: T.Text
bstToText = TE.decodeUtf8 "from bytestring to text"

txtHead :: Char
txtHead = T.head "First" --returns 'F'

txtTail :: T.Text
txtTail = T.tail "First"

t8 = decode "[1,2,3]" :: Maybe [Int]

tokTest = 
    [ Comment "hey"
    , LeftParen 
    , RightParen ]

tokTest1 = getToken tokTest

main :: IO ()
main = do
    putStrLn $ show $ fac 5
    putStrLn $ show tokTest1
    putStrLn "(car '(234324fsdfds-sdfdsfsdf3.5))"
    putStrLn $ show t1
    putStrLn $ show t1'
    putStrLn "(car (cdr (cons '(-3. . +5) (cons 4.5 23423432))))"
    putStrLn $ show t2'
    putStrLn "test 3)"
    putStrLn $ show t3'  
    putStrLn $ show t4
    putStrLn $ show t5
    putStrLn $ show t6
    putStrLn $ show txtTail
    putStrLn $ show t7
    putStrLn $ show t8
    putStrLn $ "Encode:  " ++ (show (encode (Person { name = "Joe", age = 12 })))
    putStrLn $ "Decode:  " ++ (show (decode "{ \"name\": \"Joe\", \"age\": 12 }" :: Maybe Person))
    putStrLn "done"