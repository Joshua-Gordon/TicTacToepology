module Standard where

import Board
import Data.List

standardRule :: Rule
standardRule board = any full $ diagonals board ++ rows board ++ cols board
                    where
                      full [a,b,c] = a == b && b == c
                      diagonals [[a1,_,b1],
                                 [_ ,c,_ ],
                                 [a2,_,b2]] = [[a1,c,b2],[b1,c,a2]]
                      rows = id
                      cols = transpose
