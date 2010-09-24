import LifeEngine

type Pos = (Int, Int)

goto :: Pos -> IO()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

next :: Cells -> IO ()
next css  = do c <- getChar
               goto (0, 0)
               putStr "\ESC[2J"
               if c `elem` "xXqQ\ESC"
                  then putStrLn "Bye"
                  else do css' <- nextGeneration css
                          mapM_ print css'
                          next css'

main :: IO ()
main = do putStr "\ESC[2J"
          goto (0, 0)
          css <- initLife (10, 10)
          mapM_ print css
          next css
