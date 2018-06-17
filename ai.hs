module Ai where
import Data.List
import Board

toggleMove::Move->Move
toggleMove X = O
toggleMove O = X

data Outcome = W Move | P |D deriving(Eq,Show)-- Won inPlay Draw

data Trie a = Leaf a | Node a [(Trie a)]

instance (Show a) => Show (Trie a) where
  show t = shw 0 t

shw:: (Show a) => Int -> Trie a -> [Char]
shw depth (Leaf l) = concat ["\n",(repList "    " depth),"L ",show(l)]
shw depth (Node n ts) = concat (["\n",(repList "    " depth),"N ",show(n)]  ++ [shw (depth+1) t | t<-ts])

repList::[a] -> Int -> [a]
repList xs 0 = []
repList xs n = xs ++ (repList xs (n-1))

preff:: Move -> (Outcome,(Int,Int)) -> (Outcome,(Int,Int)) -> Ordering
preff m (o1,p1) (o2,p2) = compare (rank o1) (rank o2)
  where
    rank:: Outcome -> Int
    rank (W n) | n == m = 3
               | otherwise = 0
    rank P = 2
    rank D = 1

ai :: Rule -> Board -> Move ->  (Int,Int)
ai rule board turn = loc
  where
    gs = gameStates rule board turn :: Trie(Outcome,Board,Move)
    options = zip (getOuts gs) (validMoves board):: [(Outcome,(Int,Int))]
    bestMove
      | (length options) > 0 = maximumBy (preff turn) options ::(Outcome,(Int,Int))
      |otherwise = error "You called the ai on a terminated game"
    (out,loc) = bestMove

getOuts::Trie(Outcome,Board,Move) -> [Outcome]
getOuts (Node n ns) = [o | (o,_,_)  <- (fmap getCont ns)]
getOuts (Leaf _ ) = []

getCont:: Trie a -> a
getCont (Node x _) = x
getCont (Leaf l) = l


gameStates:: Rule -> Board -> Move ->Trie(Outcome,Board,Move)
gameStates r b m = treeClass r (moveTree r b m)

moveTree :: Rule -> Board -> Move -> Trie (Board,Move)
moveTree rule board m
  | (((length newMoves)>0)&& (not isWin)) = Node (board,m) [(moveTree rule nb nm) | nb <- newBoards]
  |otherwise = Leaf (board,m)
  where
    newMoves = fmap (move m) (validMoves board)
    newBoards = [ move_ board | move_ <- newMoves]
    isWin = rule board
    nm = toggleMove m

validMoves::Board -> [(Int,Int)]
validMoves board = [ (x,y) | x<-xl , y<-yl , ((board!!x)!!y)==Empty ]
  where
    xl = [0..(length board)-1]
    yl = [0..(length (board!!0))-1]

treeClass:: Rule -> Trie (Board,Move) -> Trie(Outcome,Board,Move)
treeClass rule (Node (b,m) ns) = (Node (newOutcome,b,m) newSub)
  where
    newOutcome
      |canWin = W m
      |areHeked = W (toggleMove m)
      |otherwise = P
    newSub = fmap (treeClass rule) ns ::[Trie(Outcome,Board,Move)]
    subOutComes = [ o | (Node (o,_,_) _) <- newSub] ++ [o | Leaf (o,_,_) <- newSub]:: [Outcome]
    canWin = or [o==(W m) | o <- subOutComes] :: Bool
    areHeked = and [o==(W (toggleMove m)) | o<-subOutComes ] ::Bool
treeClass rule (Leaf (b,m)) = Leaf (oc,b,m)
  where
    oc
      | (rule b) = W (toggleMove m)
      | otherwise = D
