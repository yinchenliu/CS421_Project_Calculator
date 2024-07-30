module Parser where
    
import Control.Applicative
import Data.Char

data Parser a = Parser(String -> [(a,String)] )

run :: Parser a -> (String->[(a,String)])
run (Parser f) =f

item:: Parser Char
item = Parser(\inp-> case inp of 
                []->[]
                (x:xs) ->([(x,xs)])
                )
instance Functor Parser where
    -- fmap:: (a->b) -> Parser a -> Parser b
    fmap f p = Parser(\inp-> case run p inp of
                            [] -> []
                            [(x,inp1)]->[(f x,inp1)]
                             )

instance Applicative Parser where
    --pure :: a -> Parser a
    -- <*> :: Parser (a->b) -> Parser a -> Parser b
    pure x = Parser(\inp -> [(x,inp)])
    
    pf <*> px = Parser (\inp -> case run pf inp of
                                [] -> []
                                [(f,inp1)] -> run (fmap f px) inp1
                                )


instance Monad Parser where
    -- return a :: Parser a
    return = pure

    -- (>>=) :: Parser a -> (a->Parser b)-> Parser b
    px >>= fp = Parser (\inp -> case run px inp of 
                                []->[]
                                [(x,inp1)]-> run (fp x) inp1)


instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb = Parser (\inp -> case run pa inp of 
                                [] -> run pb inp
                                [(x,inp1)]->[(x,inp1)])

sat :: (Char->Bool)->Parser Char
sat pred = do x<- item
              if pred x then return x else empty

--for parsing float
fList::[Char]
fList= ".0123456789"

isFloat = sat (\c->elem c fList)

digit :: Parser Char
digit = sat isDigit

lower:: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char->Parser Char
char x = sat (==x)

string :: String-> Parser String
string [] = return []
string (x:xs) = do y<-char x
                   z<-string xs
                   return (y:z)

ident :: Parser String
ident = do x<-lower
           xs<- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs<- some digit
         return (read(xs))

fat:: Parser Float
fat = do xs<- some isFloat
         return (read (xs))

space :: Parser ()
space = do many (sat isSpace)
           return ()

int:: Parser Int
int = do string "-"
         xs<- nat
         return (-xs)
         <|> nat

float :: Parser Float
float = do 
    string "-"
    xs<-fat
    return (-xs)
    <|> fat

token :: Parser a -> Parser a
token p = do space
             xs<- p 
             space 
             return xs

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol:: String -> Parser String
symbol s = token (string s)


