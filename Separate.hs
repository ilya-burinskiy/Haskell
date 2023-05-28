import Control.Monad.Reader
import Control.Monad.Writer.Lazy

separate :: (a -> Bool) ->
            (a -> Bool) ->
            [a] ->
            WriterT [a] (Writer [a]) [a]

separate pred1 pred2 (x : xs)
  | pred1 x && pred2 x = do
    tell [x]
    lift (tell [x])
    separate pred1 pred2 xs
  | pred1 x = do
    tell [x]
    separate pred1 pred2 xs
  | pred2 x = do
    lift (tell [x])
    separate pred1 pred2 xs
  | otherwise = do
    xs' <- separate pred1 pred2 xs
    return (x : xs')

separate _ _ [] = return []
