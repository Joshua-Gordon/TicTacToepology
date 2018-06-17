import Board
import Standard
import Control.Monad
import Klein

main :: IO ()
main = let x = generateAllBoards (2,2)
           in
           print "bbrian"--mapM print x

generateAllBoards :: (Int,Int) -> [Board]
generateAllBoards (x,y) = [chunk x list | list <- getLists (x*y)]
                          where
                          getLists :: Int -> [[Move]] --Generates all lists of n moves
                          getLists n = replicateM n [X,O]

                          chunk :: Int -> [a] -> [[a]]
                          chunk n l = take n l : chunk n (drop n l)


k3::Rule
k3 = klein 3 3

makeBoard:: Int -> [(Int,Int)] -> Board
makeBoard dim ms = (doMoves ms) (emptyBoard dim)


doMoves:: [(Int,Int)] -> Board -> Board
doMoves (m:ms) b = (doMoves ms (move X m b))
doMoves [] b = b


basic::Board
basic = makeBoard 3 [(2,0),(0,1),(0,2)]
