{-# LANGUAGE InstanceSigs #-}

module Parser
  ( -- * Parser data type and its constructor
    Parser(..)

    -- * Functions
  , evalParser
  , execParser
  , ok
  , eof
  , satisfy
  , element
  , stream
  , digit
  , natural
  , int
  , zeroOrOne
  , satisfy'
  , element'
  ) where

import Control.Applicative
import Data.Char

newtype Parser s a 
  = Parser { runParser :: [s] -> Maybe (a, [s]) -- ^ Function that takes a stream of elements of the type
                                                -- @s@ and returns a pair of the result and the rest of 
                                                -- the stream, wrapped in the @Maybe@ context
           }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser $ \l -> case p l of
    Nothing      -> Nothing
    Just (a, l') -> Just (f a, l')

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \l -> Just (x, l)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser pf <*> Parser pa = Parser $ \l -> case pf l of
    Nothing      -> Nothing
    Just (f, l') -> case pa l' of
      Nothing       -> Nothing
      Just (a, l'') -> Just (f a, l'') 

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser p >>= f = Parser $ \l -> case p l of
    Nothing      -> Nothing
    Just (a, l') -> runParser (f a) l'

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser x <|> Parser y = Parser $ \l -> case x l of
    Nothing      -> y l
    Just (a, l') -> Just (a, l')

-- | Return the resulting value after parsing the initial given stream of values of the type @s@,
-- if the parsing was successful.
evalParser :: Parser s a -> [s] -> Maybe a
evalParser p ss = fmap fst (runParser p ss)

-- | Return the remaining stream of values of the type @s@ after parsing the initial given stream of values,
-- if the parsing was successful.
execParser :: Parser s a -> [s] -> Maybe [s]
execParser p ss = fmap snd (runParser p ss)

----------------------

-- | Return a parser that doesn't consume any of the elements from the given stream of values
-- and always returns a guaranteed successful parsing denoted by the 'unit' data type.
ok :: Parser s ()
ok = pure ()

-- | Return a parser that checks whether the input stream of values is empty or not, and returns the
-- corresponding result based on that speculation; the successful parsing is denoted by the 'unit' data type.
eof :: Parser s ()
eof = Parser $ \l -> case l of
  [] -> Just ((), [])
  _  -> Nothing

-- | Return a parser that checks if the first element of the input stream of values satisfies the
-- given predicate and returns the pair of the first element and the rest of the stream wrapped
-- in a @Just@ data constructor; in any other case returns @Nothing@.
satisfy :: (s -> Bool) -> Parser s s
satisfy predicate = Parser $ \l -> case l of
  []       -> Nothing
  (x : xs) -> if predicate x then Just (x, xs) else Nothing

-- | Check if the given element of the type @s@ is the first element of the input stream of values
-- inside a parser.
element :: Eq s => s -> Parser s s
element c = satisfy (== c)

-- | Check if the given stream of values of the type @s@ is the prefix of the input stream of values
-- inside a parser. 
stream :: Eq s => [s] -> Parser s [s]
stream []       = return []
stream (x : xs) = element x >>= \x' -> stream xs >>= \xs' -> return (x' : xs')

-- | Return a parser that checks if the first element of the input stream of values if a digit.
digit :: Parser Char Char
digit = satisfy isDigit

-- | Return a parser that checks if the first non-zero number of elements are digits and parses the
-- string to a non-negative integer. The parsing via 'read' is guaranteed to be successful at all times and 
-- does not throw an error if the parsing of the string is invalid.
natural :: Parser Char Int
natural = some digit >>= return . read

-- | Return a parser that returns an integer, i.e. either a non-negative natural number or a natural number
-- with a '-' character appended to its head.
int :: Parser Char Int
int = (element '-' >> natural >>= return . negate)
  <|> natural 

-- | Given a parser, return a parser with the possibly erroneous result if the parsing is unsuccessful.
zeroOrOne :: Parser s a -> Parser s (Maybe a)
zeroOrOne p = (p >>= return . Just) <|> pure Nothing

---------------------
-- extra functions --
---------------------

-- | Return a parser with the same semantics as the @satisfy@ parser, except it does NOT consume the first 
-- element from the input stream of values.
satisfy' :: (s -> Bool) -> Parser s ()
satisfy' predicate = Parser $ \l -> case l of
  []       -> Nothing
  (x : xs) -> if predicate x then Just ((), l) else Nothing

-- | Check if the given element of the type @s@ is the first element of the input stream of values
-- inside a parser. The parser does NOT consume the first element from the input stream of values.
element' :: Eq s => s -> Parser s ()
element' c = satisfy' (== c)