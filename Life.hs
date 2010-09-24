import IO
import Text.Regex
import Network.CGI
import Random
import Control.Monad.Trans (lift)
import Control.Parallel.Strategies (rnf)
import System.Posix.Files (fileExist)
import LifeEngine


evalCells :: String -> Cells
evalCells = splitRegexMap evalRow "(\\],\\[|\\[\\[)|\\]\\]"

evalRow :: String -> Row
evalRow = splitRegexMap read "\\[|\\]|,"

splitRegexMap :: (String -> a) -> String -> String -> [a]
splitRegexMap f regex s = map f $ filter (not . null) $ splitRegex (mkRegex regex) s

generateId :: IO String
generateId = do
  g <- getStdGen
  return $ concatMap (show . (\n -> mod n 10)) $ take 8 (randoms g :: [Int])

jsonId :: String -> String
jsonId id = "{\"id\":" ++ id ++ "}"

jsonCells :: String -> Cells -> String
jsonCells id css = "{\"id\":" ++ id ++ ",\"cells\":" ++ (show css) ++ "}"

dataPath :: String -> String
dataPath id = "./data/" ++ id

writeCells :: String -> Cells -> IO ()
writeCells id css = writeFile (dataPath id) $ show css

readCells :: String -> IO Cells
readCells id = do s <- bracket (openFile (dataPath id) ReadMode)
                               hClose
                               (\h -> hGetContents h >>= (\d -> rnf d `seq` return d))
                  return $ evalCells s

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  maybeId <- getInput "id"
  case maybeId of
       Nothing -> do
         id <- lift $ generateId
         output $ jsonId id
       Just id -> do
         maybeW <- getInput "w"
         width <- do case maybeW of
                          Nothing -> return 40
                          Just w  -> return $ read w
         maybeH <- getInput "h"
         height <- do case maybeH of
                           Nothing -> return 40
                           Just h  -> return $ read h
         exist <- lift $ fileExist ("./data/" ++ id)
         if exist
            then do css <- lift $ readCells id
                    css' <- lift $ nextGeneration css
                    lift $ writeCells id css'
                    output $ jsonCells id css
            else do css <- lift $ initLife (width, height)
                    lift $ writeCells id css
                    output $ jsonCells id css

main :: IO ()
main = runCGI $ handleErrors cgiMain
