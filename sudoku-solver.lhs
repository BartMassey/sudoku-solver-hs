Sudoku Solver in Haskell
---
Copyright (c) 2018 Bart Massey

This is a tutorial Sudoku solver inspired by
<https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/>.
My goals:

* Brush up my Haskell, which has gone stale from neglect.

* Show my approach to this problem, inspired by several
  Sudoku solvers I've written in the past.

* Write a Haskell tutorial as a Literate Haskell Markdown
  document in Bird format.

---

So we want to solve Sudokus. Let's start by grabbing the
Haskell machinery we will need.

* We plan to read a Sudoku grid from an input file and write
  it to an output file, for which `System.IO` and
  `Data.Char` will be helpful.

* We will be working with maps and sets a lot, so bring them
  in.  We want to use `Data.Map.Strict` for performance. We
  need to play games with the operators to avoid using weird
  module-qualified versions where there are name collisions.

* It's hard to write a Haskell program without `Data.List`
  in play.

> import Data.Char
> import qualified Data.Map.Strict as M
> import Data.Map.Strict ((!))
> import Data.List hiding ((\\), (!))
> import Data.Ord
> import qualified Data.Set as S
> import Data.Set ((\\))
> import System.IO

Our internal board representation will be a map from
row-column coordinate to integer digit, with no entries for
empty cells. This representation is easier to work with and
more efficient than lists-of-lists in Haskell.

Let's set up aliases for some of the basic types. This makes
the code more readable and is free.

> type Coord = (Int, Int)
> type Digit = Int
> type Board = M.Map Coord Digit

A good place to start is always the grid-reading code. This
can be tested independently. Our reader assumes that the
input is well-formatted. This is probably a bad assumption,
but for a tutorial example it should be fine.

> readGrid :: String -> Board
> readGrid boardString =
>   M.fromList $ map makeDigit $ filter (isDigit . snd) $ cells
>   where
>     cells = concatMap numberCol $ zip [0..] $ lines boardString
>     numberCol (r, s) = map (\(c, d) -> ((r, c), d)) $ zip [0..] s
>     makeDigit (c, d) = (c, digitToInt d)

Rows, columns and boxes are at the heart of Sudoku. We will
treat these uniformly using sets.

Let us start by building the set of all possible board
coordinates.

> boardCoords :: S.Set Coord
> boardCoords = S.fromList $ [(r, c) | r <- [0..8], c <- [0..8]]

It is useful to be able to find the set of board coordinates
that have not yet been set on a given board.

> openCoords :: Board -> S.Set Coord
> openCoords board = boardCoords \\ M.keysSet board

Given a specific coordinate `(r, c)`, we will need the set
of coordinates that might conflict with it. The box
coordinates are the tricky part: we want the specified
coordinate to map into the same box as the cell in question.

> conflictCells :: Coord -> S.Set Coord
> conflictCells (r, c) =
>     S.filter conflicting boardCoords
>     where
>        conflicting (r', c') = r' == r || c' == c || inBox (r', c') where
>          inBox (r', c') = inb r r' && inb c c' where
>              inb x x' = x `div` 3 == x' `div` 3

Just as we have the set of all board coordinates, it is
useful to have the set of all digits.

> allDigits :: S.Set Digit
> allDigits = S.fromList [1..9]

Next, we build the set of digits that are present on
board at a given set of coordinates.

> digits :: Board -> S.Set Coord -> S.Set Digit
> digits board coords =
>     S.fromList $ M.elems $ M.restrictKeys board coords

Finally we build the set of open digits for a given coord:
the set of values that do not conflict with the board at
that spot.

> openDigits :: Board -> Coord -> S.Set Digit
> openDigits board coord =
>     allDigits \\ digits board (conflictCells coord)

We will build a map that gives the open digits for each open
location. We will call such a map an `Openings`, since we
need a name.

> type Openings = M.Map Coord (S.Set Digit)

This construction is a little wasteful: we move between map,
set and list representations freely. Hopefully the compiler
will optimize all this into something sane.

> availableOpenings :: Board -> Openings
> availableOpenings board =
>     M.fromAscList $
>     map (\c -> (c, openDigits board c)) $
>     S.toAscList $
>     openCoords board

All the machinery is in place. We are ready to write a
solver. This is hard to read because of the bottom-to-top
right-to-left reading of Haskell. We'll break it into
pieces.

It is important to remember that a board might
have no solution. This is true at top-level: it is even more
true when trying to extend a partial solution.

> solve :: Board -> Maybe Board

The base case of solver recursion is that there is no more
work left to do. In this case a solution has been
found. Otherwise we try to extend the partial solution.

> solve board =
>     case M.null openings of
>       True -> Just board
>       False ->

Our strategy is to pick a location and try each the possible
digits at that location. If one of them works, we have
solved the problem. Otherwise we have failed.

The choice of location is a heuristic that can make a huge
performance difference. Our heuristic chooses the location
with:

* Minimum number of choices to try (most constrained first).

* Break ties by minimum smallest value (least constraining
  first).

* Break ties by minimum in left-to-right top-to-bottom
  coordinate order (locality).

This may not be optimal, but it should be good enough.

>           tryDigits $ S.toAscList candidates
>           where
>             (coord, candidates) =
>                 minimumBy (comparing width) $ M.toAscList openings
>                 where
>                   width (c, ds) = (S.size ds, S.toAscList ds, c)

We loop through the digits in ascending order (least
constraining first) trying to find one that yields a
solution.

>             tryDigits [] = Nothing
>             tryDigits (v : vs) =
>                 case solve board' of
>                   Nothing -> tryDigits vs
>                   soln -> soln
>                 where
>                   board' = M.insert coord v board

Here's the available openings needed clear up at the top of
the function.

>     where
>       openings = availableOpenings board

When we have found a solution we will need to print
it. Let's keep things out of the `IO` monad and just
transform a board into a string. We handle the slightly
over-general case of partially-solved board because it is
easy and useful for debugging.

> gridString :: Board -> String
> gridString board =
>     unlines $ do
>       row <- [0..8]
>       return $ do
>         col <- [0..8]
>         return $ formatCell (row, col)
>     where
>       formatCell coord =
>           case M.lookup coord board of
>             Nothing -> '.'
>             Just d -> intToDigit d

Finally, we can put it all together into a program which
reads a board, invokes the solver, and prints the result.

> main :: IO ()
> main = do
>   boardString <- getContents
>   let board = readGrid boardString
>   case solve board of
>     Just b -> putStr $ gridString b
>     Nothing -> putStrLn "no solution"


Future Work
===

* This program seems to work on a few examples, but it
  hasn't been adequately tested.

* The performance seems to be adequate, but benchmarking
  would probably be a good idea.

* There is an obvious performance improvement: rather than
  calculating the openings at each step, just delete the
  chosen digit from the choices everywhere it conflicts.
  This is a bit harder to code and read, but is more
  realistic.

* Extending the code to return all possible solutions
  instead of just the first-found would be
  straightforward. Sudokus are supposed to have a unique
  solution, but it would be good to check.

License
===

This work is made available under the "MIT License".  Please
see the file `LICENSE` in the source distribution for
license terms.
