module Board where

data Move = X | O
  deriving (Eq, Show)


type Board = [[Move]]

type Rule = [[Move]] -> Bool
