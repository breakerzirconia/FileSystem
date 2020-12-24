{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module FileSystemEssentials 
  ( -- * Data types
    FileSystemException(..)
  , Permission(..)
  , Directory(..)
  , File(..)
  , Command(..)

  -- * Lens functions
  , fileName
  , fileContent
  , filePath
  , filePermissions
  , fileExtension
  , fileSize
  , fileCreated
  , fileModified
  , fileAccessed
  , directoryName
  , directoryContent
  , directoryPath
  , directoryPermissions
  , name
  , arguments
  , flags


  -- * Constants and examples
  , commands
  , example

  -- * Functions
  , maybeLToList
  , lower
  , upper
  , space
  , parseS
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens
import Data.Char
import Data.Map.Strict
import Data.Maybe
import Data.Time.Clock
import Parser
import System.IO.Unsafe

data FileSystemException 
  = FileSystemException
  | FileDoesn'tExistException
  | DirectoryDoesn'tExistException
  | PathDoesn'tExistException
  | FileAlreadyExistsException
  | DirectoryAlreadyExistsException
  | PathAlreadyExistsException
  deriving (Eq, Ord, Show, Read)

instance Exception FileSystemException

type Bytes = Integer

data Permission = Readable | Writable | Executable deriving (Eq, Show, Ord, Read)

data File = File { _fileName        :: String
                 , _fileContent     :: String
                 , _filePath        :: FilePath
                 , _filePermissions :: [Permission]
                 , _fileExtension   :: String
                 , _fileSize        :: Bytes
                 , _fileCreated     :: UTCTime
                 , _fileModified    :: UTCTime
                 , _fileAccessed    :: UTCTime
                 } deriving (Eq, Show)

$(makeLenses ''File)

data Directory = Directory { _directoryName        :: String
                           , _directoryContent     :: [Either File Directory]
                           , _directoryPath        :: FilePath
                           , _directoryPermissions :: [Permission]
                           } deriving (Eq, Show)

$(makeLenses ''Directory)

data Command = Command { _name      :: String
                       , _arguments :: [String]
                       , _flags     :: [(String, [String])]
                       } deriving (Eq, Show)

$(makeLenses ''Command)

-- | The map containing the list of the possible number of arguments per a command.
commands :: Map String [Int]
commands = fromList [ ("cd", [0, 1])
                    , ("ls", [0, 1])
                    , ("cat", [1])
                    , ("touch", [1, 2])
                    , ("mkDir", [1])
                    , ("rmFile", [1])
                    , ("rmDir", [1])
                    , ("write", [2])
                    , ("append", [2])
                    , ("clear", [1])
                    , ("size", [1])
                    , ("permissions", [1])
                    , ("modified", [1])
                    , ("accessed", [1])
                    , ("extension", [1])
                    ]


----------------------


example :: Directory
example = Directory "ROOT"
                    [ Right (Directory "Dir1"
                                       []
                                       "ROOT/Dir1"
                                       [Readable, Writable])
                    , Left (File "File1.txt"
                                 "OMNIDIAMOND"
                                 "ROOT/File1.txt"
                                 [Readable, Writable]
                                 ".txt"
                                 11
                                 (unsafePerformIO getCurrentTime)
                                 (unsafePerformIO getCurrentTime)
                                 (unsafePerformIO getCurrentTime))
                    , Right (Directory "Dir2"
                                       [ Left (File "File2.hs"
                                                    "module File2 where\n\nf :: Int\nf = 22"
                                                    "ROOT/Dir2/File2.txt"
                                                    [Readable]
                                                    ".hs"
                                                    38
                                                    (unsafePerformIO getCurrentTime)
                                                    (unsafePerformIO getCurrentTime)
                                                    (unsafePerformIO getCurrentTime))
                                       ]
                                       "ROOT/Dir2"
                                       [Readable, Writable])
                    ]
                    "ROOT"
                    [Readable, Writable]


----------------------


maybeLToList :: Maybe [a] -> [a]
maybeLToList m = if isJust m then fromJust m else []

lower :: Parser Char Char
lower = satisfy isLower

upper :: Parser Char Char
upper = satisfy isUpper

space :: Parser Char Char
space = satisfy isSpace

argName :: Parser Char String
argName = do c <- satisfy (\c -> isAlphaNum c || isSymbol c || (isPunctuation c && c /= '-'))
             rest <- many $ satisfy (\c -> isAlphaNum c || isSymbol c || isPunctuation c)
             return $ c : rest 

argNameWS :: Parser Char String
argNameWS = do c <- satisfy (\c -> isAlphaNum c || isSymbol c || 
                                      isSpace c || (isPunctuation c && c /= '-' && c /= '"'))
               rest <- many $ satisfy (\c -> isAlphaNum c || isSymbol c || 
                                                isSpace c || (isPunctuation c && c /= '"'))
               return $ c : rest

flagName :: Parser Char Char
flagName = satisfy isAlphaNum

parseS :: Parser Char Command
parseS = do commandName <- some $ satisfy isAlpha
            a <- zeroOrOne parseA
            f <- zeroOrOne parseF
            return $ Command commandName (maybeLToList a) (maybeLToList f)

parseA :: Parser Char [String]
parseA = do a <- (do some space
                     Parser.element '\"'
                     arg <- argNameWS
                     Parser.element '\"'
                     return arg
              <|> do some space
                     arg <- argName
                     return arg)
            rest <- zeroOrOne parseA
            return $ a : (maybeLToList rest)
     <|> return []

parseF :: Parser Char [(String, [String])]
parseF = do some space
            Parser.element '-'
            flag <- some flagName
            a <- zeroOrOne parseA
            f <- zeroOrOne parseF
            return $ (flag, (maybeLToList a)) : (maybeLToList f)
     <|> return []
