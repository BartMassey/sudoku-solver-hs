-- Copyright (c) 2018 Bart Massey

-- Sudoku Solver in Haskell
import Data.Char
import qualified Data.Map as M
import Data.List hiding ((\\))
import qualified Data.Set as S
import Data.Set ((\\))
import System.IO


type Coord = (Int, Int)
type Value = Int
type Board = M.Map Coord Value

readGrid :: String -> Board
readGrid boardString =
  M.fromList $ map makeValue $ filter (isDigit . snd) $ cells
  where
    cells = concatMap numberCol $ zip [0..] $ lines boardString
    numberCol (r, s) = map (\(c, v) -> ((r, c), v)) $ zip [0..] s
    makeValue (c, v) = (c, digitToInt v)

values :: (Coord -> Bool) -> Board -> S.Set Value
values f board = S.fromList $ map snd $ M.toList $
                 M.filterWithKey (\c _ -> f c) board

rowValues :: Coord -> Board -> S.Set Value
rowValues (r, _) = values (\(r', _) -> r' == r)

colValues :: Coord -> Board -> S.Set Value
colValues (_, c) = values (\(_, c') -> c' == c)

boxValues :: Coord -> Board -> S.Set Value
boxValues (r, c) =
    values inBox
    where
      inBox (r', c') = inb r r' && inb c c'
      inb x x' = x `div` 3 == x' `div` 3

boxCoords :: S.Set Coord
boxCoords = S.fromList $ [(r, c) | r <- [0..8], c <- [0..8]]

openCoords :: Board -> S.Set Coord
openCoords board = boxCoords \\ M.keysSet board

allValues :: S.Set Value
allValues = S.fromList [1..9]
            
openValues :: Coord -> Board -> S.Set Value
openValues coord board =
    ((allValues \\ r) \\ c) \\ b
    where
      r = rowValues coord board
      c = colValues coord board
      b = boxValues coord board

main :: IO ()
main = do
  boardString <- getContents
  let board = readGrid boardString
  let c = (3, 4)
  print $ rowValues c board
  print $ colValues c board
  print $ boxValues c board
  print $ openValues c board
