import Text.Parsec
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Eq, Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Eq, Show)

parsePerson :: String -> Either Error Person
parsePerson input =
    let parser = runExceptT $ do
                    firstName <- parseFirstName
                    mbNewLine <- lift $ optionMaybe $ try $ char '\n'
                    when (isNothing mbNewLine) $ throwE ParsingError

                    lastName <- parseLastName
                    mbNewLine <- lift $ optionMaybe $ try $ char '\n'
                    when (isNothing mbNewLine) $ throwE ParsingError

                    age <- parseAge

                    return $ Person firstName lastName age
     in case parse parser "" input of
            Right res -> res
            Left _ -> Left ParsingError 

parseFirstName = do
    res <- lift $ optionMaybe $ try $ string "firstName"
    when (isNothing res) $ throwE IncompleteDataError

    lift $ optional $ char ' '
    res <- lift $ optionMaybe $ try $ char '='
    when (isNothing res) $ throwE ParsingError

    lift $ optional $ char ' '
    mbFirstName <- lift $ optionMaybe $ many1 $ noneOf "\n"
    when (isNothing mbFirstName) $ throwE ParsingError

    return (fromJust mbFirstName)

parseLastName = do
    res <- lift $ optionMaybe $ try $ string "lastName"
    when (isNothing res) $ throwE IncompleteDataError 

    lift $ optional $ char ' '
    res <- lift $ optionMaybe $ try $ char '='
    when (isNothing res) $ throwE ParsingError

    lift $ optional $ char ' '
    mbLastName <- lift $ optionMaybe $ many1 $ noneOf "\n"
    when (isNothing mbLastName) $ throwE ParsingError
    return (fromJust mbLastName)

parseAge = do
    res <- lift $ optionMaybe $ try $ string "age"
    when (isNothing res) $ throwE IncompleteDataError

    lift $ optional $ char ' '
    res <- lift $ optionMaybe $ try $ char '='
    when (isNothing res) $ throwE ParsingError

    lift $ optional $ char ' '
    mbAge <- lift $ optionMaybe $ try $ many1 digit
    when (isNothing mbAge) $ do
        res <- lift $ optionMaybe $ many1 $ noneOf "\n"
        when (isNothing res) $ throwE IncompleteDataError
        throwE $ IncorrectDataError (fromJust res)
    return $ read $ fromJust mbAge
