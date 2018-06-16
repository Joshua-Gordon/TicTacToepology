import Board
import Standard
import Control.Monad

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
