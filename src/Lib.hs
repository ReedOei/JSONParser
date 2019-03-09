module Lib where

import Control.Applicative hiding (many)

import Data.Maybe

data Parser a = Parser (String -> Maybe (String, a))

instance Functor Parser where
    fmap f (Parser parse) = Parser $ \s -> fmap ((\(a,b) -> (a, f b)) $ parse s

instance Applicative Parser where
    pure v = Parser $ \s -> Just (s, v)

    (Parser f) <*> (Parser g) = Parser $ \s -> do
        (rest, func) <- f s
        (final, x) <- g rest
        pure (final, func x)

instance Monad Parser where
    return = pure

    (Parser f) >>= g = Parser $ \s -> do
        (rest, temp) <- f s
        parser <- g temp
        pure $ parse rest

instance Alternative Parser where
    empty = Parser $ const Nothing

    (Parser f) <|> (Parser g) =
        Parser $ \s -> f s <|> g s

char c = Parser $ char' c

char' :: Char -> String -> Maybe (String, Char)
char' _ [] = Nothing
char' c (x:xs)
    | c == x = Just (xs, c)
    | otherwise = Nothing

string :: String -> Parser String
string = mapM char

choice = foldl (<|>) empty
oneOf = choice . map char

digit = oneOf "12343567890"
letter = oneOf $ ['a'..'z'] ++ ['A'..'Z']
symbolChar = oneOf "!@#$%^&*(),./<>?:;-' _=+"

sepEndBy1 :: Parser a -> Parser b -> Parser [b]
sepEndBy1 sep parser = do
    v <- parser

    let p = do
        sep
        xs <- sepEndBy sep parser
        pure $ v:xs
    p <|> pure [v]

sepEndBy sep parser = sepEndBy1 sep parser <|> pure []

many parser = do
    v <- optional parser
    case v of
        Nothing -> pure []
        Just x -> (:) <$> pure x <*> many parser

between a b parser = do
    a
    v <- parser
    b
    pure v

whitespace = many $ oneOf " \t\r\n"

symbol = between whitespace whitespace

surrounded start end = between (symbol (string start)) (symbol (string end))

parse (Parser parser) str = parser str

data JSONObject = IntVal Integer
                | Null
                | StringVal String
                | BoolVal Bool
                | Array [JSONObject]
                | Pairs [(String, JSONObject)]
    deriving (Show, Eq)

jsonObject = jsonBool <|>
             jsonInt <|>
             jsonStr <|>
             jsonNull <|>
             jsonArray <|>
             jsonPairs

jsonPairs = Pairs <$> surrounded "{" "}" (sepEndBy (symbol (string ",")) kvPair)

kvPair = do
    key <- jsonStr
    symbol $ string ":"
    val <- jsonObject

    case key of
        StringVal str -> pure (str, val)

jsonNull = symbol (string "null") >> pure Null

jsonArray = Array <$> surrounded "[" "]" (sepEndBy (symbol (string ",")) jsonObject)

jsonStr = StringVal <$> between quote quote (many (letter <|> digit <|> symbolChar))
    where
        quote = symbol $ char '\"'

jsonBool = (symbol (string "true") >> pure (BoolVal True)) <|>
           (symbol (string "false") >> pure (BoolVal False))

jsonInt = do
    first <- digit
    rest <- many digit
    pure $ IntVal $ read $ first:rest

