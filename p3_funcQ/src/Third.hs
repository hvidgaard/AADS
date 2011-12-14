module Third
( ThirdQueue (..)
, makelist ) where
import Queue

-- Queues with pre-evaluation
data ThirdQueue = ThirdQueue {
		left :: [Int],
		right :: [Int],
		lefthat :: [Int]
	} deriving (Show)

makelist :: ThirdQueue
makelist = ThirdQueue {left=[], right=[], lefthat=[]}

instance Queue ThirdQueue where
	insert e ThirdQueue {left=l, right=r, lefthat=lhat} =
		makeq ThirdQueue {left=l, right=e:r, lefthat=lhat}
	remove ThirdQueue {left=[], right=r, lefthat=lhat} = 
		( Nothing, makeq ThirdQueue {left=[], right=r, lefthat=lhat} )
	remove ThirdQueue {left=l:ls, right=r, lefthat=lhat} = 
		( Just l, makeq ThirdQueue {left=ls, right=r, lefthat=lhat} )

makeq :: ThirdQueue -> ThirdQueue
makeq ThirdQueue {left=l, right=r, lefthat=[]} =
	ThirdQueue {left=l', right=[], lefthat=l'}
	where l' = rot l r []
makeq ThirdQueue {left=l, right=r, lefthat=_:lhats} =
	ThirdQueue {left=l, right=r, lefthat=lhats}

rot :: [Int] -> [Int] -> [Int] -> [Int]
rot [] (r:_) a = r:a
rot (l:ls) (r:rs) a = l:( rot ls rs (r:a) )
rot _ _ _ = error "This should not happen"