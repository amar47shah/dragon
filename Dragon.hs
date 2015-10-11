module Dragon where

import Prelude hiding (Left, Right)

dragon :: Integer -> String
dragon n | n < 0 = ""
dragon 0         = "Fa"
dragon n         = extend . dragon . pred $ n

extend :: String -> String
extend = foldr extend' ""
  where extend' 'a' ex = "aRbFR" ++ ex
        extend' 'b' ex = "LFaLb" ++ ex
        extend'  c  ex = c : ex

trace :: Int -> String -> Cursor
trace n s = go n start s
  where go n' c _ | n' < 1 = c
        go n' c ('F':ds)   = go (n' - 1) (forward c) ds
        go n' c ('L':ds)   = go n'       (left c)    ds
        go n' c ('R':ds)   = go n'       (right c)   ds
        go n' c ('a':ds)   = go n'       c           ds
        go n' c ('b':ds)   = go n'       c           ds
        go _  c _          = c

type Cursor = (Integer, Integer, Direction)

start :: Cursor
start = (0, 0, Up)

forward :: Cursor -> Cursor
forward (x, y, Up   ) = (x    , y + 1, Up   )
forward (x, y, Right) = (x + 1, y    , Right)
forward (x, y, Down ) = (x    , y - 1, Down )
forward (x, y, Left ) = (x - 1, y    , Left )

left :: Cursor -> Cursor
left (x, y, d) = (x, y, rotateL d)

right :: Cursor -> Cursor
right (x, y, d) = (x, y, rotateR d)

rotateR :: Direction -> Direction
rotateR Up = Right
rotateR Right = Down
rotateR Down = Left
rotateR Left = Up

rotateL :: Direction -> Direction
rotateL = rotateR . rotateR . rotateR

data Tree = LeafA | LeafB | NodeA Memo Tree Tree | NodeB Memo Tree Tree
data Direction = Up | Right | Down | Left deriving Show
data Memo = Memo Integer (Integer, Integer) (Direction -> Direction)


steps :: Tree -> Integer
steps LeafA = 0
steps LeafB = 0
steps (NodeA _ a b) = 1 + steps a + steps b
steps (NodeB _ a b) = 1 + steps a + steps b

facing :: Tree -> Direction -> Direction
facing LeafA = id
facing LeafB = id
facing (NodeA _ a b) = facing a . facing b . uTurn
facing (NodeB _ a b) = facing a . facing b . uTurn

uTurn :: Direction -> Direction
uTurn Up = Down
uTurn Right = Left
uTurn Down = Up
uTurn Left = Right

--travel :: Tree -> (Integer, Integer)
--travel LeafA = (0, 0)
--travel LeafB = (0, 0)
--travel NodeA =
