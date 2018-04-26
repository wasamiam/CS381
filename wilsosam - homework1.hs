-- Samuel Wilson - OSUID: wilsosam

module MiniLogo where

-- 1(a):
data Cmd = Pen Mode
         | Moveto Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | Comb Cmd Cmd

data Mode = Up | Down
data Pos = I Int | S String
type Pars = [String]
type Vals = [Int]

-- 1(b):
-- def vector (x1, y1, x2, y2) pen up; moveto (x1, y1); pen down; moveto (x2, y2); pen up
x1 = ""
y1 = ""
x2 = ""
y2 = ""

vector = Def "vector" [x1, y1, x2, y2] (Comb (Comb (Comb (Pen Up) (Moveto (S x1) (S y1) ) ) (Pen Down) ) (Comb (Moveto (S x2) (S y2) ) (Pen Up) ) )

-- 1(c):
-- steps :: Int -> Cmd
-- steps 0 = (Pen Up)
-- steps n = Comb() (steps (n-1) )

-- 2(a):
data Circut = Cir Gates Links
type Gates = [(Int, Gate)]
data Gate = And | Or | Xor | Not
type Links = [(Int, Int, Int, Int)]

-- 2(b):