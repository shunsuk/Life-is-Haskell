import LifeEngine

next :: Cells -> IO ()
next css  = do ln <- getLine
               css' <- return $ nextGeneration css
               mapM_ print css'
               next css'

main :: IO ()
main = do css <- initLife 10 10
          mapM_ print css
          next css
