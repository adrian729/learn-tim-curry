-- program to copy a file --
import System.Environment (getArgs)

main :: IO ()
main = do
  -- read command-line arguments
  [file1, file2] <- getArgs

  -- copy file contents
  str <- readFile file1
  writeFile file2 str
