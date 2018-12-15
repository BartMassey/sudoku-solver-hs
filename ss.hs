-- Copyright (c) 2018 Bart Massey

-- Sudoku Solver in Haskell
import Data.Char
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List hiding ((\\), (!))
import Data.Ord
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
values f board =
    S.fromList $
    M.elems $
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

type Openings = M.Map Coord (S.Set Value)

availableOpenings :: Board -> Openings
availableOpenings board =
    M.fromAscList $
    map (\c -> (c, openValues c board)) $
    S.toAscList $
    openCoords board

solve :: Openings -> Board -> Maybe Board
solve openings board | M.null openings = Just board
solve openings board =
    tryCells values
    where
      (coord, values) = (cMin, S.toAscList vsMin)
          where
            (cMin, vsMin) =
                minimumBy (comparing width) $ M.assocs openings
                where
                  width (c, vs) = (S.size vs, S.toAscList vs, c)
      open = openValues coord board
      tryCells [] = Nothing
      tryCells (v : vs) =
          case solve openings' board' of
            Nothing -> tryCells vs
            soln -> soln
          where
            (openings', board') =
                case S.size open of
                  0 -> error "solve: internal error: empty values"
                  1 -> (M.delete coord openings, board')
                  _ -> (M.adjust (S.delete v) coord openings, board')
                where
                  board' = M.insert coord v board

gridString :: Board -> String
gridString board =
    unlines $ do
      row <- [0..8]
      return $ do
        col <- [0..8]
        return $ intToDigit $ board ! (row, col)

main :: IO ()
main = do
  boardString <- getContents
  let board = readGrid boardString
  case solve (availableOpenings board) board of
    Just b -> putStr $ gridString b
    Nothing -> putStrLn "no solution"
