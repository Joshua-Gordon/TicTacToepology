import Board
import Standard
import Control.Monad
import Ai
import Standard

makeBoard:: Int -> [(Int,Int)] -> Board
makeBoard dim ms = (doMoves ms) (emptyBoard dim)

doMoves:: [(Int,Int)] -> Board -> Board
doMoves (m:ms) b = (doMoves ms (move X m b))
doMoves [] b = b

basic::Board
basic = makeBoard 3 [(0,0),(2,1),(0,2)]

sai = ai standardRule
