module Util where
import Data.Text (Text, unsnoc, empty)

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
