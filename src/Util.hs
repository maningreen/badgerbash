-- a module full of useful, pure functions
module Util where
import Data.Text (Text, unsnoc, empty)
import qualified Data.Text as T
import Brick (Widget, (<+>), AttrName, withAttr, str)

foldFind :: (b -> a -> (b, Bool)) -> b -> [a] -> (b, Maybe a)
foldFind _ acc [] = (acc, Nothing)
foldFind f acc (x : xs)
  | done = (acc', Just x)
  | otherwise = foldFind f acc' xs
 where
  (acc', done) = f acc x

initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

textInitSafe :: Text -> Text
textInitSafe i = case unsnoc i of
  Nothing -> empty 
  Just (t, _) -> t

applyAttrToString :: (Char -> AttrName) -> Text -> (Text, [AttrName])
applyAttrToString f s = (s, T.foldr g [] s)
  where
    g x a = f x : a

applyAttrToListW :: (a -> Widget n) -> (a -> AttrName) -> [a] -> Widget n
applyAttrToListW _ _ [] = str ""
applyAttrToListW g f (x:xs) = withAttr attr (g x) <+> applyAttrToListW g f xs
  where
    attr = f x

breakChunks :: Int -> [a] -> [[a]]
breakChunks _ [] = []
breakChunks x xs = pre : breakChunks x post 
  where
    (pre, post) = splitAt x xs

wrapString :: Int -> String -> [String]
wrapString = breakChunks

-- merge two lists, where xs has priority of ys
-- returns a new list of length = max (length xs) (length ys)
-- examples:
-- >merge [3, 4, 5, 7] [3, 5, 19, 23, 22]
-- [3, 4, 5, 7, 22]
merge :: [a] -> [a] -> [a]
merge (x:xs) (_:ys) = x : merge xs ys
merge [] ys = ys
merge xs _ = xs
