--zad 3
import System.Environment
import System.IO
 
countFile :: Handle -> IO Int
countFile handle = do
  eof <- hIsEOF handle
  if eof
    then return 0
    else do
      line <- hGetLine handle
      count <- countFile handle
      return (length line + count)
 
main = do
  (firstArg : _) <- getArgs
  fileHandle <- openFile firstArg ReadMode
  count <- countFile fileHandle
  print count
  hClose fileHandle
 
 