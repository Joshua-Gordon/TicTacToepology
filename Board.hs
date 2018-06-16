module Board where

data Move = X | O | Empty
  deriving (Eq)

instance Show Move where
  show X = "X"
  show O = "O"
  show Empty = " "


type Board = [[Move]]


type Rule = [[Move]] -> Bool


emptyBoard :: Int -> Board
emptyBoard n = [[Empty | _ <- [1..n]] | _ <- [1..n]]

printBoard :: Board -> IO ()
printBoard b = let board = fmap (\row -> show row ++ "\n") b
                in putStrLn $ concat board

move :: Move -> (Int,Int) -> Board -> Board
move new (x,y) b = [b !! n | n <- [1..x]] ++ replace (b!!x) : [b !! n | n <- [x+1 .. length b - 1], n > x]
                    where
                      replace :: [Move] -> [Move]
                      replace l = [l !! m | m <- [1..y]] ++ new : [l !! m | m <- [y+1..(length l)-1]]
