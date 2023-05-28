import Control.Applicative (Alternative (..), (<|>))

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
satisfyEP :: (Char -> Bool) -> PrsEP Char

parseEP p = snd . runPrsEP p 0
satisfyEP predicate = PrsEP $ \ position str ->
  case str of
    char : strRem ->
      if predicate char then (position + 1, Right (char, strRem))
      else (position + 1, Left ("pos " ++ show (position + 1) ++ ": unexpected " ++ [char]))
    "" -> (position + 1, Left ("pos " ++ show (position + 1) ++ ": unexpected end of input"))

instance Functor PrsEP where
  fmap f parser = PrsEP $ \ position string ->
    case runPrsEP parser position string of
      (newPosition, Right (x, stringRemainder)) -> (newPosition, Right (f x, stringRemainder))
      (newPosition, Left error) -> (newPosition, Left error)

instance Applicative PrsEP where
  pure x = PrsEP $ \ position string -> (position, Right (x, string))
  fA <*> parser = PrsEP $ \ position string ->
    case runPrsEP fA position string of
      (newPosition, Right (f, stringRemainder)) ->
        case runPrsEP parser newPosition stringRemainder of
          (newPosition', Right (x, stringRemainder')) -> (newPosition', Right (f x, stringRemainder'))
          (newPosition', Left errorMsg) -> (newPosition', Left errorMsg)
      (newPosition, Left errorMsg) -> (newPosition, Left errorMsg)

instance Alternative PrsEP where
  empty = PrsEP $ \ position string -> (position, Left ("pos " ++ show position ++ ": empty alternative"))
  p1 <|> p2 = PrsEP $ \ position string ->
    case runPrsEP p1 position string of
      (newPosition, Right x) -> (newPosition, Right x)
      (newPosition, Left error) ->
        case runPrsEP p2 position string of
          (newPosition', Right x') -> (newPosition', Right x')
          (newPosition', Left error') -> if newPosition' > newPosition
                                         then (newPosition', Left error')
                                         else (newPosition, Left error)
