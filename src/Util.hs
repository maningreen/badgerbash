-- a module full of useful, pure functions
module Util where

import Brick (AttrName, Widget, str, withAttr, (<+>))

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

breakChunks :: Int -> [a] -> [[a]]
breakChunks _ [] = []
breakChunks x xs = pre : breakChunks x post
 where
  (pre, post) = splitAt (x - 1) xs

wrapString :: Int -> String -> [String]
wrapString = breakChunks

mapSnd ::  (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

zipWithM :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithM _ [] [] = []
zipWithM f (x : xs) (y : ys) = f (Just x) (Just y) : zipWithM f xs ys
zipWithM f [] (y : ys) = f Nothing (Just y) : zipWithM f [] ys
zipWithM f (x : xs) [] = f (Just x) Nothing : zipWithM f xs []

mergeb :: (a -> b -> b) -> [a] -> [b] -> [b]
mergeb f (x:xs) (y:ys) = f x y : mergeb f xs ys
mergeb _ [] ys = ys
mergeb _ _ [] = undefined

mergea :: (a -> b -> a) -> [a] -> [b] -> [a]
mergea f (x:xs) (y:ys) = f x y : mergea f xs ys
mergea _ xs [] = xs
mergea _ [] _ = undefined

snoc :: [a] -> a -> [a]
snoc a = (a ++) . return
