module Flushable where

import Control.Monad.IO.Class
import FileSystem
import System.IO

-- | @Flushable@ typeclass. Allows to flush @stdout@ upon having run the provided action.
class MonadIO m => Flushable m where
  flush :: IO a -> m ()
  flush action = liftIO action >> liftIO (hFlush stdout)

instance Flushable FileSystemIO 

-- | @FlushActions@ typeclass. Allows to combine the @flush@ function with some of the standard IO actions.
class Flushable m => FlushActions m where
  fPutStr :: String -> m ()
  fPutStr = flush . putStr
  fPutStrLn :: String -> m ()
  fPutStrLn = flush . putStrLn
  fPrint :: Show a => a -> m ()
  fPrint = flush . print

instance FlushActions FileSystemIO