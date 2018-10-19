-- Defining types.
type Peg = String
type Move = (Peg, Peg)
-- Returns the Towers of Hanoi solution for an arbitrary number of disks.
getHanoiMoves :: Integer -> Peg -> Peg -> Peg -> [Move]
getHanoiMoves 0 a b c = []
-- Copying disks from A to C using B and then A as temporary storage.
getHanoiMoves n a b c = (getHanoiMoves (n - 1) a c b) ++ [(a,c)] ++ (getHanoiMoves (n -1) b a c)
