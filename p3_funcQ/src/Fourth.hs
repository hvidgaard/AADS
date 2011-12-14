module Fourth
( Fourth (..)
, makelist ) where
import Queue

-- Queues as paired lazy lists
data FourthQueue = FourthQueue {
		left :: [Int],
		right :: [Int],
		leftLength :: Int,
		rightLength :: Int
	} deriving (Show)

makelist :: FourthQueue
makelist = FourthQueue {left=[], right=[], leftLength=0, rightLength=0}

instance Queue FourthQueue where
	insert e FourthQueue {left=l, right=r, leftLength=lengthL, rightLength=lengthR} =
		makeq FourthQueue {left=l, right=e:r, leftLength=lengthL, rightLength=lengthR+1}
	remove FourthQueue {left=l:ls, right=r, leftLength=lengthL, rightLength=lengthR} =
		( l,	makeq FourthQueue {left=ls, right=r, leftLength=lengthL-1, rightLength=lengthR} )

makeq :: FourthQueue -> FourthQueue
makeq FourthQueue {left=l, right=r, leftLength=leftLength, rightLength=rightLength}
	| rightLength <= leftLength   = id
	| rightLength == leftLength+1 = FourthQueue {
			left        = rot l r [],
			right       = [],
			leftLength  = l+r,
			rightLength = 0 }

rot :: [Int] -> [Int] -> [Int] -> [Int]
rot [] r a = head r:a
rot l:ls r:rs a  = l:( rot  ls rs (r:a) )
	



