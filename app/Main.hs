module Main where

import Control.Monad
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.Char
import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe
import FileSystemEssentials
import FileSystem
import Flushable
import Parser
import System.Directory
import System.Exit
import System.IO

main :: IO ()
main = runFileSystemIO' run

-- | Perform the @FileSystemIO@ action.
run :: FileSystemIO ()
run = do dir <- fsio getDir
         fPutStr $ "[|RUNNING ON " ++ List.map toUpper (show thisOS) ++ "|]--" ++ dir ++ "> "
         s <- liftIO $ getLine
         let parsed = evalParser parseS s
         if isNothing parsed
         then liftIO $ throwIO FileSystemException
         else do let command = fromJust parsed 
                 if command^.name == "exit" 
                 then fPutStrLn "EXITING..." >> liftIO exitSuccess
                 else analyzeCommand command >> run

-- | Given the Command, return the respective file system action.
analyzeCommand :: (FileSystemActions m, FlushActions m) => Command -> m ()
analyzeCommand command = if (commands !? (command^.name)) /= Nothing && 
                            List.length (command^.arguments) `elem` (commands ! (command^.name))
                         then case command^.name of
  "ls" -> case List.length (command^.arguments) of
            0 -> ls' >>= fPrint >> liftIO (return ())
            1 -> ls (head (command^.arguments)) >>= fPrint >> liftIO (return ())
  "cd" -> case List.length (command^.arguments) of
            0 -> cd' >>= fPutStrLn >> liftIO (return ())
            1 -> oneArgDiscard cd
  "cat" -> cat (head (command^.arguments)) >>= fPutStrLn >> liftIO (return ())
  "touch" -> case List.length (command^.arguments) of
            1 -> oneArgDiscard touch
            2 -> twoArgsDiscard touchP
  "mkDir" -> oneArgDiscard mkDir
  "rmFile" -> oneArgDiscard rmFile
  "rmDir" -> oneArgDiscard rmDir
  "write" -> twoArgsDiscard write
  "append" -> twoArgsDiscard append
  "clear" -> oneArgDiscard clear
  "size" -> FileSystem.size (head (command^.arguments)) >>= fPrint >> liftIO (return ())
  "permissions" -> permissions (head (command^.arguments)) >>= fPrint >> liftIO (return ())
  "modified" -> modified (head (command^.arguments)) >>= fPrint >> liftIO (return ())
  "accessed" -> accessed (head (command^.arguments)) >>= fPrint >> liftIO (return ())
  "extension" -> extension (head (command^.arguments)) >>= fPutStrLn >> liftIO (return ())
                         else liftIO $ putStrLn "Unknown command entered..." 
    where
      zeroArgsDiscard :: (FileSystemActions m, MonadIO m) => m b -> m ()
      zeroArgsDiscard f = f >> liftIO (return ())

      oneArgDiscard :: (FileSystemActions m, MonadIO m) => (String -> m b) -> m ()
      oneArgDiscard f = f (head (command^.arguments)) >> liftIO (return ())
      
      twoArgsDiscard :: (FileSystemActions m, MonadIO m) => (String -> String -> m b) -> m ()
      twoArgsDiscard f = f (head (command^.arguments)) (last (command^.arguments)) >> liftIO (return ())
