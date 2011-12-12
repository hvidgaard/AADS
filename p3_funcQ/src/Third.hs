module Third
( push
, pop
, inject
, eject
, size
, makelist
) where
import Queue

data ThirdQueue = ThirdQueue {
		left :: [Int],
		right :: [Int],
		leftLength :: Int,
		rightLength :: Int
	} deriving (Show)

makelist :: Int -> ThirdQueue
makelist x = ThirdQueue {left=[x], right=[], leftLength=1, rightLength=0}

instance Queue ThirdQueue where
	inject y ThirdQueue {left=h, right=t, leftLength=l, rightLength=r}
		| l > r = ThirdQueue {
				left=h,
				right=y:t,
				leftLength=l,
				rightLength=r+1 }
		| otherwise  = ThirdQueue {
				left=h ++ reverse (y:t),
				right=[],
				leftLength=l+r+1,
				rightLength=0 }
	
	pop ThirdQueue {left=[], right=[], leftLength=l, rightLength=r} =
		( Nothing, ThirdQueue {left=[], right=[], leftLength=l, rightLength=r} )
	pop ThirdQueue {left=[], right=_, leftLength=_, rightLength=_} = error "Should not happen"
	
	pop ThirdQueue {left=h@(x:xs), right=t, leftLength=l, rightLength=r}
		| l > r = ( Just x, ThirdQueue {
				left=xs,
				right=t,
				leftLength=l-1,
				rightLength=r} )
		| otherwise = ( Just x, ThirdQueue {
				left=(xs ++ reverse t),
				right=[],
				leftLength=l-1+r,
				rightLength=0} )
	
	size ThirdQueue {left=h, right=t} = length h + length t
	
	push _ _ = error "Not implemented"
	eject _ = error "Not implemented"