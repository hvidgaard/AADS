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
	insert e FourthQueue {left=l, right=r, balance=b, size=s} =
		makeq FourthQueue {left=l, right=e:r, balance=b+1, size=s+1}
	remove FourthQueue {left=[], right=r, balance=b, size=s} =
		( Nothing, makeq FourthQueue {left=[], right=r, balance=b, size=s} )
	remove FourthQueue {left=l:ls, right=r, balance=b, size=s} =
		( Just l, makeq FourthQueue {left=ls, right=r, balance=b+1, size=s-1} )

makeq :: FourthQueue -> FourthQueue
makeq FourthQueue {left=l, right=r, balance=1, size=s} =
	FourthQueue {left = rot l r [], right = [], balance = -s, size = s }
makeq queue = queue

rot :: [Int] -> [Int] -> [Int] -> [Int]
rot [] (r:_) a = r:a
rot (l:ls) (r:rs) a = l:( rot ls rs (r:a) )
rot _ _ _ = error "This should not happen"