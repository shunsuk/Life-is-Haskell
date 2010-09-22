module LifeEngine (Row, Cells, initLife, nextGeneration) where

import Random

type Row = [Int]
type Cells = [Row]

randomCell :: IO Int
randomCell = getStdRandom (randomR (0, 1))

initRow :: Int -> IO [Int]
initRow w = sequence . take w . repeat $ randomCell 

initLife :: Int -> Int -> IO Cells
initLife w h = sequence . take h . repeat $ initRow w

cellScore :: Int -> Int -> Cells -> Int
cellScore x y css = foldr (\(a, b) n -> n + cellValue (x + a) (y + b) css) 0
  [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= 0 || b /= 0]

cellValue :: Int -> Int -> Cells -> Int
cellValue x y css
  | x < 0 || w <= x || y < 0 || h <= y = 0
  | otherwise                          = css !! y !! x
      where h = length css
            w = length $ css !! 0

applyRule :: Int -> Int -> Cells -> Int
applyRule x y css
  | score == 2  = cellValue x y css
  | score == 3  = 1
  | otherwise   = 0
      where score = cellScore x y css

nextRow :: Int -> Cells -> Row
nextRow y css = map (\x -> applyRule x y css) [0..(w - 1)]
  where w = length (css !! y)

nextGeneration :: Cells -> Cells
nextGeneration css = foldr (\y cs -> nextRow y css : cs) [] [0..(h - 1)]
  where h = length css
