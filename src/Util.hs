-- a module full of useful, pure functions
module Util where

import Brick (AttrName, Widget, str, withAttr, (<+>))

type Time = Float

foldFind :: (b -> a -> (b, Bool)) -> b -> [a] -> (b, Maybe a)
foldFind _ acc [] = (acc, Nothing)
foldFind f acc (x : xs)
  | done = (acc', Just x)
  | otherwise = foldFind f acc' xs
 where
  (acc', done) = f acc x

-- a full version of the impartial function `init`
-- ex:
-- ```haskell
-- >initSafe [3, 4, 5, 5]
-- [3, 4, 5]
-- ```
-- ```haskell
-- >initSafe []
-- []
-- ```
initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

applyAttrToListW :: (a -> Widget n) -> (a -> AttrName) -> [a] -> Widget n
applyAttrToListW _ _ [] = str ""
applyAttrToListW g f (x : xs) = withAttr attr (g x) <+> applyAttrToListW g f xs
 where
  attr = f x

-- mapIf (cont True) id == map
-- mapIf (cont False) _ == map
-- applies a function to all items which fufill the predicate
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf predicate f = map (\x -> if predicate x then f x else x)

-- mapIf (cont True) id == map
-- mapIf (cont False) _ == map
-- applies f() to all items which fufill the predicate
-- otherwise applies g()
mapIfElse :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapIfElse predicate f g = map (\x -> if predicate x then f x else g x)

breakChunks :: Int -> [a] -> [[a]]
breakChunks _ [] = []
breakChunks x xs = pre : breakChunks x post
 where
  (pre, post) = splitAt (x - 1) xs

wrapString :: Int -> String -> [String]
wrapString = breakChunks

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

zipWithM :: (Maybe a -> Maybe b -> Maybe c) -> [a] -> [b] -> [c]
zipWithM _ [] [] = []
zipWithM f (x : xs) (y : ys) = case f (Just x) (Just y) of
  Just gamma -> gamma : zipWithM f xs ys
  Nothing -> zipWithM f xs ys
zipWithM f [] (y : ys) = case f Nothing (Just y) of
  Just gamma -> gamma : zipWithM f [] ys
  Nothing -> zipWithM f [] ys
zipWithM f (x : xs) [] = case f (Just x) Nothing of
  Just gamma -> gamma : zipWithM f xs []
  Nothing -> zipWithM f xs []

mergeb :: (a -> b -> b) -> [a] -> [b] -> [b]
mergeb f (x : xs) (y : ys) = f x y : mergeb f xs ys
mergeb _ [] ys = ys
mergeb _ _ [] = undefined

mergea :: (a -> b -> a) -> [a] -> [b] -> [a]
mergea f (x : xs) (y : ys) = f x y : mergea f xs ys
mergea _ xs [] = xs
mergea _ [] _ = undefined

merge :: [a] -> [a] -> [a]
merge (x : xs) (_ : ys) = x : merge xs ys
merge xs [] = xs
merge [] ys = ys

snoc :: [a] -> a -> [a]
snoc a = (a ++) . return

fromBool :: (Num a) => Bool -> a
fromBool True = 1
fromBool _ = 0

count :: (Eq a, Foldable t) => a -> t a -> Int
count x xs = foldl f 0 xs
 where
  f acc = (acc +) . fromBool . (x ==)

(//) :: (Integral a) => a -> a -> a
_ // 0 = 0
a // b = a `div` b

roundTo :: (RealFrac a, Integral b) => b -> a -> a
roundTo n x = (fromInteger . round $ x * (10 ^ n)) / (10.0 ^^ n)

trim :: String -> String
trim [] = []
trim (' ' : ' ' : xs) = ' ' : trim xs
trim (' ' : xs) = ' ' : trim xs
trim (x : xs) = x : trim xs

log10 :: (Floating a) => a -> a
log10 = logBase 10
