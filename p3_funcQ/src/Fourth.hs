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
makeq (FourthQueue left right 1 size) =
	FourthQueue (rot left right []) [] (-size) size
makeq queue = queue

rot :: [Int] -> [Int] -> [Int] -> [Int]
rot [] (r:_) a = r:a
rot (l:ls) (r:rs) a = l:( rot ls rs (r:a) )
rot _ _ _ = error "This should not happen"