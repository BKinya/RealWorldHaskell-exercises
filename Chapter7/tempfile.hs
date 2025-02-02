import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception (catch, IOException)
import Control.Exception(finally)


main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempName temph =
 do
   putStrLn "Welcome to tempfile.hs"
   putStrLn $ "I have a temporary file at " ++ tempName

   pos <- hTell temph
   putStrLn $ "My initial position is " ++ show pos

   -- Write some data to the temporary file
   let tempData = show [1..10]
   putStrLn $ "Writing one line containing " ++
              show (length tempData) ++ "bytes: " ++
              tempData
   hPutStrLn temph tempData

   pos <- hTell temph
   putStrLn $ "After writing, my new position is " ++ show pos

   -- seek the beginning of the file and display it
   putStrLn $ "The file content is:"
   hSeek temph AbsoluteSeek 0

   -- hGetContents performs a lazy read of the entire file
   c <- hGetContents temph

   -- copy file byte for byte to the stdout
   putStrLn c

   -- Display the content as Haskell literal
   putStrLn $ "Which could be expressed as this Haskell Literal:"
   print c

   
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
  do
    tempDir <- catch getTemporaryDirectory
                     (\(e :: IOException) -> return ".")
    (tempFile, temph) <- openTempFile tempDir pattern


    finally (func tempFile temph)
            (do hClose temph
                removeFile tempFile)
