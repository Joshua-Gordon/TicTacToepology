import Board
import Standard
import Control.Monad
import Klein

main :: IO ()
main = let filt = getFiltered 5
           valids = fmap checkNumbers filt
           masked = [board | (bool,board) <- zip valids filt, bool]
           printable =  concat [printBoard f ++ "\n" | f <- masked]
           numValid = length $ filter id valids
       in
       do
         print valids
         print (length filt)
         putStrLn printable
         print numValid

getFiltered :: Int -> [Board]
getFiltered x = getStalemate (x,x) (klein x x)

getStalemate :: (Int,Int) -> Rule -> [Board]
getStalemate (x,y) r = let boards = generateAllBoards (x,y)
                           valid = [b | (check,b) <- zip (fmap checkNumbers boards) boards, check]
                       in filter (not . r) valid

generateAllBoards :: (Int,Int) -> [Board]
generateAllBoards (x,y) = [chunk x list | list <- getLists (x*y)]

getLists :: Int -> [[Move]] --Generates all lists of n moves
getLists n = replicateM n [X,O]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)

checkNumbers :: Board -> Bool
checkNumbers b = let c = count X b - count O b
                 in
                 c == 1 || c == -1 || c == 0
                 where
                 count :: Move -> Board -> Int
                 count m = foldr ((+) . length . filter (== m)) 0
