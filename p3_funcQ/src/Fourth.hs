module Fourth
( push
, pop
, inject
, eject
, size
, makelist
) where
import Queue

data FourthQueue = FourthQueue {
		left :: [Int],
		right :: [Int],
		leftLength :: Int,
		rightLength :: Int
	} deriving (Show)

makelist :: Int -> FourthQueue
makelist x = FourthQueue {left=[x], right=[], leftLength=1, rightLength=0}

instance Queue FourthQueue where
	inject y FourthQueue {left=h, right=t, leftLength=l, rightLength=r}
		| l > r = FourthQueue {
				left=h,
				right=y:t,
				leftLength=l,
				rightLength=r+1 }
		| otherwise  = FourthQueue {
				left=h ++ reverse (y:t),
				right=[],
				leftLength=l+r+1,
				rightLength=0 }
	
	pop FourthQueue {left=[], right=[], leftLength=l, rightLength=r} =
		( Nothing, FourthQueue {left=[], right=[], leftLength=l, rightLength=r} )
	pop FourthQueue {left=[], right=_, leftLength=_, rightLength=_} = error "Should not happen"
	
	pop FourthQueue {left=h@(x:xs), right=t, leftLength=l, rightLength=r}
		| l > r = ( Just x, FourthQueue {
				left=xs,
				right=t,
				leftLength=l-1,
				rightLength=r} )
		| otherwise = ( Just x, FourthQueue {
				left=(xs ++ reverse t),
				right=[],
				leftLength=l-1+r,
				rightLength=0} )
	
	size FourthQueue {left=h, right=t} = length h + length t
	
	push _ _ = error "Not implemented"
	eject _ = error "Not implemented"