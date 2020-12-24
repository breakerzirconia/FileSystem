{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module FileSystem where

import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Either
import Data.IORef
import Data.List
import Data.List.Split
import Data.Time.Clock
import FileSystemEssentials
import System.Directory
import System.Directory.Internal
import System.FilePath.Posix     hiding (pathSeparator, splitPath)
import System.Info
import System.IO
import System.IO.Unsafe

-- | Check whether the first list is a prefix of the second.
prefix :: Eq a => [a] -> [a] -> Bool
prefix (x : xs) (y : ys) = x == y && prefix xs ys
prefix []       _        = True
prefix _        []       = False
 
-- | Check whether the first list is a suffix of the second.
suffix :: Eq a => [a] -> [a] -> Bool
suffix a b = prefix (reverse a) (reverse b) 

-- | Return the OS type depending on the operating system your machine is running on.
thisOS :: OS
thisOS = if prefix "mingw" os then Windows else UnixLike

-- | The OS data type, containing only two nullary constructors: Windows and UnixLike
data OS 
  = UnixLike 
  | Windows 
  deriving (Eq, Show, Ord, Read)

-- | The real file system that works in the IO monad.
newtype FileSystemIO a 
  = FileSystemIO { unFileSystemIO :: ReaderT (IORef FilePath) IO a -- ^ Reader that accepts the mutable @FilePath@
                                                                   -- represengin the working directory
                                                                   -- and returns an IO action 
                 } deriving (Functor, Applicative, Monad)

instance MonadIO FileSystemIO where
  liftIO :: IO a -> FileSystemIO a
  liftIO = fsio . lift

-- | Synonymous with the @FileSystemIO@ data constructor.
fsio :: ReaderT (IORef FilePath) IO a -> FileSystemIO a
fsio = FileSystemIO

-- | Given the @FileSystemIO@ and the mutable working directory, return an IO action.
runFileSystemIO :: FileSystemIO a -> (IORef FilePath) -> IO a
runFileSystemIO = runReaderT . unFileSystemIO

-- | Given the @FileSystemIO@, get the current working directory in the IO action and call
-- @runFileSystemIO@.
runFileSystemIO' :: FileSystemIO a -> IO a
runFileSystemIO' fsio = do currentDir <- getCurrentDirectory 
                           dir <- newIORef currentDir
                           runFileSystemIO fsio dir 

-- | Initialize @FileSystemIO@ with the current working directory. 
initFileSystemIO :: FileSystemIO ()
initFileSystemIO = FileSystemIO $ do givenDir <- ask
                                     currentDir <- lift getCurrentDirectory
                                     dir <- lift $ newIORef currentDir
                                     newDir <- lift $ readIORef dir
                                     lift $ writeIORef givenDir newDir
                                     return ()

type FileName = String
type Text = String

-- | Check whether the given @FilePath@ is absolute in accordance with the OS.
isAbsolutePath :: FilePath -> Bool
isAbsolutePath filePath = if thisOS == Windows 
                          then ':' `elem` filePath
                          else head filePath == '/'

-- | Check whether the given @FilePath@ is relative in accordance with the OS.
isRelativePath :: FilePath -> Bool
isRelativePath = not . isAbsolutePath

-- | Return the path separator in accordance with the OS.
pathSeparator :: Char
pathSeparator = if thisOS == Windows then '\\' else '/'

-- | Trim the whitespaces in the given @String@.
trim :: String -> String
trim = go . go
  where 
    go = reverse . dropWhile isSpace

-- | Merge the two @FilePath@s into the resulting @FilePath@ in the IO action. The second @FilePath@
-- may contain ".." and ".".
mergeFilePathsIO :: FilePath -> FilePath -> Char -> IO FilePath
mergeFilePathsIO a b c = if isAbsolutePath b
                       then do bool <- case c of
                                         'f' -> doesFileExist b
                                         'd' -> doesDirectoryExist b
                                         'p' -> doesPathExist b
                                         '_' -> return True
                               if bool then return b else throwIO FileSystemException
                       else do let bs = map trim $ splitPath b
                                   as = map trim $ splitPath a
                                   cs = go as bs
                               bool <- case c of
                                         'f' -> doesFileExist cs
                                         'd' -> doesDirectoryExist cs
                                         'p' -> doesPathExist cs
                                         '_' -> return True
                               if bool then return cs else throwIO FileSystemException
  
go xs (".." : ys) = go (init xs) ys
go xs ("."  : ys) = go xs ys
go xs (y : ys)    = go (xs ++ [y]) ys
go xs []          = (intercalate [pathSeparator] xs)

-- | Split the @FilePath@ into the list of directories via @pathSeparator@.
splitPath :: FilePath -> [String]
splitPath = splitOn [pathSeparator]

-- | Transform the @Permissions@ data type into the list of @Permission@.
perms :: Permissions -> [Permission]
perms p = let result = []
              x = if executable p then Executable : result else result
              w = if writable p   then Writable   : x      else x
              r = if readable p   then Readable   : w      else w
          in r 

-- | Class of the actions performed in the instantiated file system.
class Monad m => FileSystemActions m where
  -- | Given a @FilePath@, list all the content inside it.
  ls          :: FilePath -> m [String]

  -- | List all the content inside the current working directory.
  ls'         :: m [FilePath]
  ls' = ls ""

  -- | Change the working directory in accordance with the given @FilePath@.
  cd          :: FilePath -> m ()

  -- | Return thr current working directory.
  cd'         :: m FilePath

  -- | Return the content inside the file.
  cat         :: FilePath -> m String

  -- | Create a file and set its permissions with the provided @String@.
  touchP      :: FilePath -> String -> m ()

  -- | Create a file and set its permissions with the default @String@ "rwx".
  touch       :: FilePath -> m ()
  touch = flip touchP "rwx"

  -- | Create a directory.
  mkDir       :: FilePath -> m ()

  -- | Delete the file.
  rmFile      :: FilePath -> m ()

  -- | Delete the directory.
  rmDir       :: FilePath -> m ()

  -- | Write the text in the file. If the file does already contain some text, rewrite that text with the
  -- provided string of text.
  write       :: FilePath -> Text -> m ()

  -- | Append the text in the file, preserving the previous content inside.
  append      :: FilePath -> Text -> m ()

  -- | Clear the content inside the file.
  clear       :: FilePath -> m ()
  clear = flip write "" 

  -- | Return the size of the file in bytes.
  size        :: FilePath -> m Integer

  -- | Return the list of permissions of the file or directory.
  permissions :: FilePath -> m [Permission]

  -- | Return the date when the file or directory has been last modified.
  modified    :: FilePath -> m UTCTime

  -- | Return the date when the file or directory has been last accessed.
  accessed    :: FilePath -> m UTCTime

  -- | Retun the extension of the file.
  extension   :: FilePath -> m String  

-- | Return the curent working directory wrapped in the Reader.
getDir :: ReaderT (IORef FilePath) IO FilePath
getDir = ask >>= lift . readIORef >>= return

-- | Try to perform the @action@ and handle the respective exception if it's been thrown in the @action@.
catchFSE :: a -> IO a -> IO a
catchFSE def action 
  = action `catch` (\e -> case e of FileSystemException             -> putStrLn "General FileSystemException has been caught" >> return def
                                    FileDoesn'tExistException       -> putStrLn "The file doesn't exist"                      >> return def
                                    DirectoryDoesn'tExistException  -> putStrLn "The directory doesn't exist"                 >> return def
                                    PathDoesn'tExistException       -> putStrLn "The path doesn't exist"                      >> return def
                                    FileAlreadyExistsException      -> putStrLn "The file already exists"                     >> return def
                                    DirectoryAlreadyExistsException -> putStrLn "The directory already exists"                >> return def
                                    PathAlreadyExistsException      -> putStrLn "The path already exists"                     >> return def)

instance FileSystemActions FileSystemIO where
  ls :: FilePath -> FileSystemIO [String]
  ls filePath = FileSystemIO $ do fp <- getDir
                                  merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'd')
                                  lift $ getDirectoryContents merged
  
  cd :: FilePath -> FileSystemIO ()
  cd filePath = FileSystemIO $ do dir <- ask
                                  fp <- lift $ readIORef dir 
                                  merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'd')
                                  lift $ writeIORef dir merged

  cd' :: FileSystemIO FilePath
  cd' = FileSystemIO getDir

  cat :: FilePath -> FileSystemIO String
  cat filePath = FileSystemIO $ do fp <- getDir
                                   merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'f')
                                   lift . readFile $ merged

  touchP :: FilePath -> String -> FileSystemIO ()
  touchP filePath perms = FileSystemIO $ do fp <- getDir
                                            merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath '_')
                                            lift $ writeFile merged "" 
                                            lift $ setPermissions merged 
                                                                  (Permissions ('r' `elem` perms) 
                                                                               ('w' `elem` perms)
                                                                               ('x' `elem` perms)
                                                                               True)

  mkDir :: FilePath -> FileSystemIO ()
  mkDir filePath = FileSystemIO $ do fp <- getDir
                                     merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath '_')
                                     lift $ createDirectory merged

  rmFile :: FilePath -> FileSystemIO ()
  rmFile filePath = FileSystemIO $ do fp <- getDir
                                      merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'f')
                                      lift $ removeFile merged

  rmDir :: FilePath -> FileSystemIO ()
  rmDir filePath = FileSystemIO $ do fp <- getDir
                                     merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'd')
                                     lift $ removeDirectory merged

  write :: FilePath -> Text -> FileSystemIO ()
  write filePath text = FileSystemIO $ do fp <- getDir
                                          merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'f')
                                          lift $ writeFile merged text

  append  :: FilePath -> Text -> FileSystemIO ()
  append filePath text = FileSystemIO $ do fp <- getDir
                                           merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'f')
                                           lift $ appendFile merged text

  size :: FilePath -> FileSystemIO Integer
  size filePath = FileSystemIO $ do fp <- getDir
                                    lift $ catchFSE 0 (mergeFilePathsIO fp filePath 'f' >>= getFileSize)

  permissions :: FilePath -> FileSystemIO [Permission]
  permissions filePath = FileSystemIO $ do fp <- getDir
                                           lift $ catchFSE [] (mergeFilePathsIO fp filePath 'p' >>= 
                                                               fmap perms . getPermissions)

  modified :: FilePath -> FileSystemIO UTCTime
  modified filePath = FileSystemIO $ do fp <- getDir
                                        merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'p')
                                        lift $ getModificationTime merged

  accessed :: FilePath -> FileSystemIO UTCTime
  accessed filePath = FileSystemIO $ do fp <- getDir
                                        merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'p')
                                        lift $ getAccessTime merged

  extension :: FilePath -> FileSystemIO String
  extension filePath = FileSystemIO $ do fp <- getDir
                                         merged <- lift $ catchFSE fp (mergeFilePathsIO fp filePath 'f')
                                         lift . return $ takeExtension merged

-- | The stateful & pure fake file system that may throw the @FileSystemException@. 
newtype FileSystemMock a 
  = FileSystemMock { unFileSystemMock :: ExceptT FileSystemException (State ([String], Directory)) a -- ^ Essentially a
                                      -- State that accepts the split working directory and the ROOT-directory
                                      -- and returns the possibly erroneous result indicated by @FileSystemException@. 
                   } deriving ( Functor
                              , Applicative
                              , Monad
                              , MonadError FileSystemException
                              , MonadState ([String], Directory)
                              )

-- | Given the @FileSystemMock@ and the pair of the split working directory and the ROOT-directory, return
-- the result of the @FileSystemMock@ action.
runFileSystemMock :: FileSystemMock a -> ([String], Directory) -> Either FileSystemException a
runFileSystemMock = evalState . runExceptT . unFileSystemMock

-- | Given the @FileSystemMock@, return the result of the @FileSystemMock@ action, with the ROOT-directory being
-- the @example@ from the @FileSystemEssentials.hs@ file.
runFileSystemMock' :: FileSystemMock a -> Either FileSystemException a
runFileSystemMock' fsm = (evalState . runExceptT . unFileSystemMock) fsm (["ROOT"], example)

-- | Merge the two filepaths, where the first filepath has been split, and return the resulting split filepath.
mergeFilePaths :: [String] -> FilePath -> [String]
mergeFilePaths as b = if prefix "ROOT" b
                      then splitOn "/" b
                      else let bs = splitOn "/" b
                           in go as bs
  where
    go xs (".." : ys) = go (init xs) ys
    go xs ("."  : ys) = go xs ys
    go xs (y : ys)    = if all isSpace y then go xs ys else go (xs ++ [y]) ys
    go xs []          = xs

-- | Check whether the given split filepath is present in the @Directory@, and throw @DirectoryDoesn'tExistException@
-- otherwise.
-- WARNING: DOES *NOT* SUPPORT ".." AND "."!!!
isFilePathCorrect :: [String] -> Directory -> Either FileSystemException Directory
isFilePathCorrect []            dir = Right dir 
isFilePathCorrect ("ROOT" : xs) dir = isFilePathCorrect xs dir
isFilePathCorrect (x      : xs) dir = let contents = dir^.directoryContent
                                          f = foldr (\c acc -> case c of
                                                                 Left file -> acc
                                                                 Right d -> if d^.directoryName == x
                                                                            then d : acc
                                                                            else acc) [] contents
                                      in if null f 
                                         then Left DirectoryDoesn'tExistException
                                         else isFilePathCorrect xs (head f)

-- | Check whether the given filepath is present in the ROOT-directory, traverse it and return the directory
-- upon the successful traversal of the ROOT-directory, otherwise throw @DirectoryDoesn'tExistException@.
-- Depending on the @b@ boolean flag, update the split working directory.
ultimateDirChecker :: FilePath 
                   -> Bool
                   -> ExceptT FileSystemException (State ([String], Directory)) Directory
ultimateDirChecker filePath b = do (steps, dir) <- get
                                   let merged = mergeFilePaths steps filePath
                                       checker = isFilePathCorrect merged dir
                                   case checker of
                                     Left e -> throwError e
                                     Right d -> if b
                                                then do put (merged, dir)
                                                        lift . lift $ return d
                                                else lift . lift $ return d

-- | Same as @ultimateDirChecker@, but throw @FileDoesn'tExistException@ upon failing to traverse
-- the ROOT-directory to search for the file.
ultimateFileChecker :: FilePath 
                    -> Bool
                    -> ExceptT FileSystemException (State ([String], Directory)) File
ultimateFileChecker filePath b = do let path = filter (not . all isSpace) $ splitOn "/" filePath
                                        (dirPath, fn) = (init path, last path)
                                    dir <- ultimateDirChecker (intercalate "/" dirPath) b
                                    let contents = dir^.directoryContent
                                        f = foldr (\c acc -> case c of
                                                               Left file -> if file^.fileName == fn 
                                                                            then file : acc
                                                                            else acc
                                                               Right d -> acc) [] contents
                                    if null f
                                    then throwError FileDoesn'tExistException
                                    else lift . lift . return . head $ f

-- | Transform the list of @Either File Directory@ into the list of the names of each element.
entityNames :: [Either File Directory] -> [String]
entityNames contents = map (\c -> case c of
                                    Left file -> file^.fileName
                                    Right dir -> dir^.directoryName) contents

-- | Only extract the directories from the list of @Either File Directory@.
extractDirs :: [Either File Directory] -> [Directory]
extractDirs contents = foldr (\c acc -> case c of
                                          Left file -> acc
                                          Right d -> d : acc) [] contents

-- | Only extract the files from the list of @Either File Directory@.
extractFiles :: [Either File Directory] -> [File]
extractFiles contents = foldr (\c acc -> case c of
                                           Left file -> file : acc
                                           Right d -> acc) [] contents

-- | Transform the string of permissions (e.g. "rwx") into the list of @Permission@.
perms' :: String -> [Permission]
perms' = go [] . reverse
  where
    go acc []           = acc
    go acc ('x' : rest) = go (Writable   : acc) rest
    go acc ('w' : rest) = go (Executable : acc) rest
    go acc ('r' : rest) = go (Readable   : acc) rest

-- | Create the new @File@, given the directory it's going to be created in, its name and the list of permissions.
newFile :: Directory -> String -> String -> File
newFile dir fn perms = FileSystemEssentials.File fn
                                                 ""
                                                 (dir^.directoryPath ++ "/" ++ fn)
                                                 (perms' perms)
                                                 (takeExtension fn)
                                                 0
                                                 (unsafePerformIO getCurrentTime)
                                                 (unsafePerformIO getCurrentTime)
                                                 (unsafePerformIO getCurrentTime)

-- | Create the new @Directory@, given the directory it's going to be created in and its name.
newDirectory :: Directory -> String -> Directory
newDirectory dir dn = FileSystemEssentials.Directory dn
                                                     []
                                                     (dir^.directoryPath ++ "/" ++ dn)
                                                     [Readable, Writable, Executable]

-- | Update @insertedDir@ if it's present in @origDir@.
updateDir :: Directory -> Directory -> Directory
updateDir origDir insertedDir = if origDir^.directoryPath /= insertedDir^.directoryPath
                                then over directoryContent
                                          (map (\e -> case e of
                                                        Left file -> Left file
                                                        Right dir -> Right (updateDir dir insertedDir)))
                                          origDir
                                else insertedDir

instance FileSystemActions FileSystemMock where
  ls :: FilePath -> FileSystemMock [String]
  ls filePath = FileSystemMock $ do dir <- ultimateDirChecker filePath False
                                    lift . lift . return . entityNames $ dir^..directoryContent.traversed
  
  cd :: FilePath -> FileSystemMock ()
  cd filePath = FileSystemMock $ ultimateDirChecker filePath True >> return ()

  cd' :: FileSystemMock FilePath
  cd' = FileSystemMock $ do dir <- ultimateDirChecker "" False
                            lift . lift . return $ dir^.directoryPath

  cat :: FilePath -> FileSystemMock String
  cat filePath = FileSystemMock $ do file <- ultimateFileChecker filePath False
                                     lift . lift . return $ file^.fileContent

  touchP :: FilePath -> String -> FileSystemMock ()
  touchP filePath perms = FileSystemMock $ do let steps = filter (not . all isSpace) $ splitOn "/" filePath
                                                  (dirPath, fn) = (init steps, last steps)
                                              dir <- ultimateDirChecker (intercalate "/" dirPath) False
                                              let files = extractFiles $ dir^.directoryContent
                                              if fn `elem` (map (\f -> f^.fileName) files)
                                              then throwError FileAlreadyExistsException
                                              else do (steps', dir') <- get
                                                      let newDir = over directoryContent 
                                                                        (Left (newFile dir fn perms) :) 
                                                                        dir
                                                      put (steps', updateDir dir' newDir)

  mkDir :: FilePath -> FileSystemMock ()
  mkDir filePath = FileSystemMock $ do let steps = filter (not . all isSpace) $ splitOn "/" filePath
                                           (dirPath, dn) = (init steps, last steps)
                                       dir <- ultimateDirChecker (intercalate "/" dirPath) False
                                       let dirs = extractDirs $ dir^.directoryContent
                                       if dn `elem` (map (\d -> d^.directoryName) dirs)
                                       then throwError DirectoryAlreadyExistsException
                                       else do (steps', dir') <- get
                                               let newDir = over directoryContent 
                                                                 (Right (newDirectory dir dn) :) 
                                                                 dir
                                               put (steps', updateDir dir' newDir)

  rmFile :: FilePath -> FileSystemMock ()
  rmFile filePath = FileSystemMock $ do let steps = filter (not . all isSpace) $ splitOn "/" filePath
                                            (dirPath, fn) = (init steps, last steps)
                                        dir <- ultimateDirChecker (intercalate "/" dirPath) False
                                        let files = extractFiles $ dir^.directoryContent
                                        if not (fn `elem` (map (\f -> f^.fileName) files))
                                        then throwError FileDoesn'tExistException
                                        else do let newDir = over directoryContent
                                                                  (filter (\e -> case e of
                                                                                   Left f -> f^.fileName /= fn
                                                                                   Right d -> True))
                                                                  dir
                                                (steps', dir') <- get
                                                put (steps', updateDir dir' newDir)

  rmDir :: FilePath -> FileSystemMock ()
  rmDir filePath = FileSystemMock $ do let steps = filter (not . all isSpace) $ splitOn "/" filePath
                                           (dirPath, dn) = (init steps, last steps)
                                       dir <- ultimateDirChecker (intercalate "/" dirPath) False
                                       let dirs = extractDirs $ dir^.directoryContent
                                       if not (dn `elem` (map (\d -> d^.directoryName) dirs))
                                       then throwError DirectoryDoesn'tExistException
                                       else do let newDir = over directoryContent
                                                                 (filter (\e -> case e of
                                                                                  Left f -> True
                                                                                  Right d -> d^.directoryName /= dn))
                                                                 dir
                                               (steps', dir') <- get
                                               put (steps', updateDir dir' newDir)

  write :: FilePath -> Text -> FileSystemMock ()
  write filePath text 
    = FileSystemMock $ do let steps = filter (not . all isSpace) $ splitOn "/" filePath
                              (dirPath, fn) = (init steps, last steps)
                          dir <- ultimateDirChecker (intercalate "/" dirPath) False
                          let files = extractFiles $ dir^.directoryContent
                          if not (fn `elem` (map (\f -> f^.fileName) files))
                          then throwError FileDoesn'tExistException
                          else do let newDir = over directoryContent
                                                    (map (\e -> case e of
                                                                  Left f -> Left (if f^.fileName == fn
                                                                                  then set fileSize
                                                                                           (toInteger (length text))
                                                                                           (set fileContent
                                                                                                text
                                                                                                f)
                                                                                  else f)
                                                                  Right d -> Right d))
                                                    dir
                                  (steps', dir') <- get
                                  put (steps', updateDir dir' newDir)

  append :: FilePath -> Text -> FileSystemMock ()
  append filePath text 
    = FileSystemMock $ do let steps = filter (not . all isSpace) $ splitOn "/" filePath
                              (dirPath, fn) = (init steps, last steps)
                          dir <- ultimateDirChecker (intercalate "/" dirPath) False
                          let files = extractFiles $ dir^.directoryContent
                          if not (fn `elem` (map (\f -> f^.fileName) files))
                          then throwError FileDoesn'tExistException
                          else do let newDir = over directoryContent
                                                    (map (\e -> case e of
                                                                  Left f -> Left (if f^.fileName == fn
                                                                                  then over fileSize
                                                                                            (+ toInteger (length text))
                                                                                            (over fileContent
                                                                                                  (++ text)
                                                                                                  f)
                                                                                  else f)
                                                                  Right d -> Right d))
                                                    dir
                                  (steps', dir') <- get
                                  put (steps', updateDir dir' newDir)

  size :: FilePath -> FileSystemMock Integer
  size filePath = FileSystemMock $ do file <- ultimateFileChecker filePath False
                                      lift . lift . return $ file^.fileSize

  permissions :: FilePath -> FileSystemMock [Permission]
  permissions filePath = FileSystemMock $ do file <- ultimateFileChecker filePath False
                                             lift . lift . return $ file^.filePermissions

  modified :: FilePath -> FileSystemMock UTCTime
  modified filePath = FileSystemMock $ do file <- ultimateFileChecker filePath False
                                          lift . lift . return $ file^.fileModified

  accessed :: FilePath -> FileSystemMock UTCTime
  accessed filePath = FileSystemMock $ do file <- ultimateFileChecker filePath False
                                          lift . lift . return $ file^.fileAccessed

  extension :: FilePath -> FileSystemMock String
  extension filePath = FileSystemMock $ do file <- ultimateFileChecker filePath False
                                           lift . lift . return $ file^.fileExtension