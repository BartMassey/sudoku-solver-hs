# Sudoku Solver in Haskell
Copyright (c) 2018 Bart Massey

```haskell
import Data.Char
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List hiding ((\\), (!))
import Data.Ord
import qualified Data.Set as S
import Data.Set ((\\))
import System.IO
```

```haskell
type Coord = (Int, Int)
type Value = Int
type Board = M.Map Coord Value
```

```haskell
readGrid :: String -> Board
readGrid boardString =
  M.fromList $ map makeValue $ filter (isDigit . snd) $ cells
  where
    cells = concatMap numberCol $ zip [0..] $ lines boardString
    numberCol (r, s) = map (\(c, v) -> ((r, c), v)) $ zip [0..] s
    makeValue (c, v) = (c, digitToInt v)
```

```haskell
values :: (Coord -> Bool) -> Board -> S.Set Value
values f board =
    S.fromList $
    M.elems $
    M.filterWithKey (\c _ -> f c) board
```

```haskell
rowValues :: Coord -> Board -> S.Set Value
rowValues (r, _) = values (\(r', _) -> r' == r)
```

```haskell
colValues :: Coord -> Board -> S.Set Value
colValues (_, c) = values (\(_, c') -> c' == c)
```

```haskell
boxValues :: Coord -> Board -> S.Set Value
boxValues (r, c) =
    values inBox
    where
      inBox (r', c') = inb r r' && inb c c'
      inb x x' = x `div` 3 == x' `div` 3
```

```haskell
boxCoords :: S.Set Coord
boxCoords = S.fromList $ [(r, c) | r <- [0..8], c <- [0..8]]
```

```haskell
openCoords :: Board -> S.Set Coord
openCoords board = boxCoords \\ M.keysSet board
```

```haskell
allValues :: S.Set Value
allValues = S.fromList [1..9]
```

```haskell
openValues :: Coord -> Board -> S.Set Value
openValues coord board =
    ((allValues \\ r) \\ c) \\ b
    where
      r = rowValues coord board
      c = colValues coord board
      b = boxValues coord board
```

```haskell
type Openings = M.Map Coord (S.Set Value)
```

```haskell
availableOpenings :: Board -> Openings
availableOpenings board =
    M.fromAscList $
    map (\c -> (c, openValues c board)) $
    S.toAscList $
    openCoords board
```

```haskell
solve :: Board -> Maybe Board
solve board =
    case M.null openings of
      True -> Just board
      False ->
          tryCells $ S.toAscList values
          where
            (coord, values) =
                minimumBy (comparing width) $ M.toAscList openings
                where
                  width (c, vs) = (S.size vs, S.toAscList vs, c)
            tryCells [] = Nothing
            tryCells (v : vs) =
                case solve board' of
                  Nothing -> tryCells vs
                  soln -> soln
                where
                  board' = M.insert coord v board
    where
      openings = availableOpenings board
```

```haskell
gridString :: Board -> String
gridString board =
    unlines $ do
      row <- [0..8]
      return $ do
        col <- [0..8]
        return $ intToDigit $ board ! (row, col)
```

```haskell
main :: IO ()
main = do
  boardString <- getContents
  let board = readGrid boardString
  case solve board of
    Just b -> putStr $ gridString b
    Nothing -> putStrLn "no solution"
```
