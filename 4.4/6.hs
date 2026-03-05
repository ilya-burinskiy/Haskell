import Text.Parsec
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show
parsePerson :: ExceptT Error (Parsec String ()) Person
parsePerson = do
    res <- lift $ optionMaybe $ string "firstName"
    when (isNothing res) $
        throwE IncompleteDataError

    lift $ optional $ char ' '
    res <- lift $ optionMaybe $ char '='
    when (isNothing res) $
        throwE ParsingError

    lift $ optional $ char ' '
    mbFirstName <- lift $ optionMaybe $ many1 letter
    when (isNothing res) $
        throwE ParsingError

    res <- lift $ optionMaybe $ char '\n'
    when (isNothing res) $ 
        throwE ParsingError

    res <- lift $ optionMaybe $ string "lastName"
    when (isNothing res) $
        throwE IncompleteDataError 

    lift $ optional $ char ' '
    res <- lift $ optionMaybe $ char '='
    when (isNothing res) $
        throwE ParsingError

    lift $ optional $ char ' '
    mbLastName <- lift $ optionMaybe $ many1 letter
    when (isNothing res) $
        throwE ParsingError

    res <- lift $ optionMaybe $ char '\n'
    when (isNothing res) $ 
        throwE ParsingError

    res <- lift $ optionMaybe $ string "age"
    when (isNothing res) $
        throwE IncompleteDataError

    lift $ optional $ char ' '
    res <- lift $ optionMaybe $ char '='
    when (isNothing res) $
        throwE ParsingError

    lift $ optional $ char ' '
    mbAge <- lift $ optionMaybe $ many1 digit
    when (isNothing res) $
        throwE $ 
    return $ Person (fromJust mbFirstName) (fromJust mbLastName) (read $ fromJust mbAge)

-- parsePerson :: Parsec String () Person
-- parsePerson =
--     Person
--         <$> (string "firstName"
--                 *> optional (char ' ')
--                 *> char '='
--                 *> optional (char ' ')
--                 *> many1 letter)
--         <*> (char '\n'
--                *> string "lastName"
--                *> optional (char ' ')
--                *> char '='
--                *> optional (char ' ')
--                *> many1 letter)
--         <*> (char '\n'
--                 *> string "age"
--                 *> optional (char ' ')
--                 *> char '='
--                 *> optional (char ' ')
--                 *> parseInt)

parseInt :: Parsec String () Int
parseInt = read <$> (many1 digit)
