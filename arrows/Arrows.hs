module Arrows () where

import Control.Arrow
import Data.Bifunctor
--- Def. f is strict iff f undefined = undefined.
--- This definition comes from domain theory

-- Strict:

constUndefined :: a -> b
constUndefined _ = undefined

-- id is strict

--- Non strict:

const' :: a -> Int
const' _ = 0


myPart :: [Int] -> ([Int], [Int])
myPart = (&&&) f1 f2

f1 :: [Int] -> [Int]
f1 = filter even

f2 :: [Int] -> [Int]
f2 = filter odd


myF :: [Int] -> (Int, Int)
myF xs = bimap sum sum (myPart xs)
