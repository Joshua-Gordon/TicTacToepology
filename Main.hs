import Board
import Standard
import Control.Monad
import Klein
import Standard
import Ai

sai = ai standardRule :: Board -> Move -> (Int,Int)

main::IO ()
main = loop (emptyBoard 3)

continue::Rule -> Board -> Bool
continue rule board = (not won) && (not full)
  where
    won = rule board
    full = (length (validMoves board)) == 0



loop :: Board -> IO ()
loop board1 = do
  putStrLn (printBoard board1)
  line <- getLine
  let s' = fmap read $ words line
  let x = s' !! 0
  let y = s' !! 1
  let board2 = move X (x,y) board1
  let board3 = if (continue standardRule board2) then (move O (sai board2 O) board2) else board2
  let keepPlaying = (continue standardRule board3)
  let s = if keepPlaying then loop board3 else putStrLn ((printBoard board3) ++ "good game")
  s
