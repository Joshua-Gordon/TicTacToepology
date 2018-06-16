module Klein where

import Board
import Data.List




step:: Int -> Int -> (Int,Int) -> (Int,Int)
-- dim dir start
step dim 0 (x,y)
  | x==(dim-1) = (0 , (twist y dim))
  | otherwise = (x+1,y)
step  dim 4 (x,y)
  | x == 0 = ( (dim-1), twist y dim)
  | otherwise = (x-1,y)
step dim 2 p = reflect (step dim 0 (reflect p))
step dim 6 p = reflect (step dim 4 (reflect p))
step dim dir p = (step dim (dir-1)) . (step dim (mod (dir+1) 8 )) $ p

reflect:: (Int,Int) -> (Int,Int)
reflect (x,y) = (y,x)

twist::Int->Int->Int
twist pos dim = mod (-pos) dim

genWinStates:: Int -> Int -> (Int->(Int,Int)->(Int,Int)) -> [[(Int,Int)]]
genWinStates dim winLength step = nub sorted
  where
    redundant = [ (winState (x,y) dim dir winLength) | x <-[0..(dim-1)] , y<-[0..(dim-1)], dir <-[0..7] ] :: [[(Int,Int)]]
    sorted = fmap sort redundant

winState:: (Int,Int) -> Int -> Int -> Int -> [(Int,Int)]
winState _ _ _ 0 = []
winState p dim dir len = p: (winState (step dim dir p) dim dir (len-1))

checkWin:: Board -> [(Int,Int)]-> Bool
checkWin board states = areSame moves
  where
    moves =fmap (\(x,y) -> ((board!!x)!!y)) states

areSame::[Move] -> Bool
areSame ms = (areAll X ms) || (areAll O ms)

areAll:: Move -> [Move] -> Bool
areAll s (m:ms) = (s == m) && (areAll s ms)
areAll s [] = True

klein:: Int-> Int -> Board -> Bool
klein dim winLength board = or (fmap (checkWin board) winStates )
  where
    winStates = (genWinStates dim winLength (step dim)  ) :: [[(Int,Int)]]

klein4::Rule
klein4 = klein 4 4
