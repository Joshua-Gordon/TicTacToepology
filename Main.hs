import Board
import Standard

main :: IO ()
main = let x = emptyBoard 5
           y = move X (2,2) x
           in
           printBoard y
