module Board where

data Move = X | O

type Board = [[Move]]

type Rule = [[Move]] -> Bool
