module Lib where

import Control.Applicative hiding (many)
import Control.Monad.State

import Data.List
import Data.Maybe

data Parser a = Parser (String -> Maybe (String, a))

instance Functor Parser where
    fmap f (Parser parse) = Parser $ \s -> fmap (fmap f) $ parse s

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
        let (Parser parser) = g temp
        parser rest

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
symbolChar = oneOf "!@#$%^&*(),./<>?:;-' _=+~`[]{}\\|\n\r"

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

data JSONObject = FloatVal Float
                | Null
                | StringVal String
                | BoolVal Bool
                | Array [JSONObject]
                | Pairs [(String, JSONObject)]
    deriving (Show, Eq)

jsonObject = jsonBool <|>
             jsonFloat <|>
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

digs = do
    first <- digit
    rest <- many digit
    pure $ first:rest

jsonFloat = do
    neg <- optional $ char '-'
    first <- digs
    second <- optional $ do
        char '.'
        digs

    let mul = maybe 1 (const (-1)) neg
    pure $ FloatVal $ mul * read (first ++ "." ++ fromMaybe "0" second)

data JavaClass = JavaClass String [Field]
    deriving (Show,Eq)

data Field = Field String String
    deriving (Show,Eq)

class PrettyPrint a where
    prettyPrint :: a -> String

instance PrettyPrint JavaClass where
    prettyPrint (JavaClass name fields) =
        "public class " ++ name ++ " {\n" ++
        intercalate "\n" (map (("    "++) . prettyPrint) fields) ++ "\n}"

instance PrettyPrint Field where
    prettyPrint (Field name typeName) = "private final " ++ typeName ++ " " ++ name ++ ";"

increment = do
    i <- get
    modify (+1)
    pure i

toClass :: JSONObject -> State Integer JavaClass
toClass (Pairs keyVals) = do
    fields <- mapM pairToField keyVals
    i <- increment
    pure $ JavaClass ("Test" ++ show i) fields

typeName :: JSONObject -> State Integer String
typeName (FloatVal _) = pure $ "float"
typeName (StringVal _) = pure $ "String"
typeName (BoolVal _) = pure $ "boolean"
typeName Null = pure $ "Object"
typeName (Array (obj:_)) = do
    tName <- typeName obj
    pure $ tName ++ "[]"
typeName (Pairs vals) = do
    i <- increment
    pure $ "test" ++ show i

pairToField :: (String, JSONObject) -> State Integer Field
pairToField (name, val) = do
    tName <- typeName val
    pure $ Field name tName

