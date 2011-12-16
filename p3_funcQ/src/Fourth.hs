{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Fourth
( FourthQueue (..)
, makelist ) where
import Queue

-- Queues as paired lazy lists
data FourthQueue = FourthQueue {
		left :: [Int],
		right :: [Int],
		balance :: Int,
		size :: Int
	} deriving (Show)

makelist :: FourthQueue
makelist = FourthQueue {left=[], right=[], balance=0, size=0}

instance Queue FourthQueue where
	insert e FourthQueue {..} =
		makeq (FourthQueue left (e:right) (balance+1) (size+1))
	remove FourthQueue {left=[], right, balance, size} =
		( Nothing, makeq (FourthQueue [] right balance size) )
	remove FourthQueue {left=l:ls, right, balance, size} =
		( Just l, makeq (FourthQueue ls right (balance+1) (size-1)) )

makeq :: FourthQueue -> FourthQueue
makeq (FourthQueue left right 1 size) = -- balance > 0
	FourthQueue (rot left right []) [] (-size) size
makeq queue = queue

rot :: [Int] -> [Int] -> [Int] -> [Int]
rot [] (r:_) a = r:a
rot (l:ls) (r:rs) a = l:( rot ls rs (r:a) )
rot _ _ _ = error "This should not happen"

-- Insert: Balance Size -> Balance Size (makeq transformation, when balance == 1)
-- 1:  1 1 -> -1 1
-- 2:  0 1
-- 3:  1 3 -> -3 3
-- 4: -2 4
-- 5: -1
-- 6: 0
-- 7:  1 7 -> -7 7
-- 8: -6 8
-- 9: -5
-- 10: -4
-- 11: -3
-- 12: -2
-- 13: -1
-- 14:  0
-- 15:  1 15 -> -15 15
-- 16: -14 16