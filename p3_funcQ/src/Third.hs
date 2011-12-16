{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
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
	insert e ThirdQueue {..} =
		makeq (ThirdQueue left (e:right) lefthat)
	remove ThirdQueue {left=[], right, lefthat} = 
		( Nothing, makeq (ThirdQueue [] right lefthat) )
	remove ThirdQueue {left=l:ls, right, lefthat} = 
		( Just l, makeq (ThirdQueue ls right lefthat) )

makeq :: ThirdQueue -> ThirdQueue
makeq (ThirdQueue left right []) =
	let l' = rot left right []
	in ThirdQueue l' [] l'
makeq ThirdQueue {left, right, lefthat=_:lhats} =
	ThirdQueue left right lhats

rot :: [Int] -> [Int] -> [Int] -> [Int]
rot [] (r:_) a = r:a
rot (l:ls) (r:rs) a = l:( rot ls rs (r:a) )
rot _ _ _ = error "This should not happen"