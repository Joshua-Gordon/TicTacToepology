import Board
import Standard
import Control.Monad
import Klein

main :: IO ()
main = let filt = getStalemate (4,4) (klein 4 4)
           printable =  concat [printBoard f ++ "\n" | f <- filt]
       in
       do
         print (length filt)
         putStrLn printable

getStalemate :: (Int,Int) -> Rule -> [Board]
getStalemate (x,y) r = let boards = generateAllBoards (x,y)
                       in filter (not . r) boards

generateAllBoards :: (Int,Int) -> [Board]
generateAllBoards (x,y) = [chunk x list | list <- getLists (x*y)]

getLists :: Int -> [[Move]] --Generates all lists of n moves
getLists n = replicateM n [X,O]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)
