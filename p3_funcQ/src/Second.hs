module Second
( push
, pop
, inject
, eject
, size
, makelist
) where
import Queue

data SecondQueue = SecondQueue {left :: [Int], right :: [Int]} deriving (Show)

makelist :: Int -> SecondQueue
makelist x = SecondQueue {left=[x], right=[]}

instance Queue SecondQueue where
	inject y SecondQueue {left=h, right=t} = ( SecondQueue {left=h, right=y:t} )
	pop SecondQueue {left=[], right=[]} = ( Nothing, SecondQueue {left=[], right=[]})
	pop SecondQueue {left=[], right=t} =
		let t' = reverse t
		in ( Just (head t'), SecondQueue {left=tail t', right=[]} )
	pop SecondQueue {left=x:xs, right=t} = ( Just x, SecondQueue {left=xs, right=t} )
	
	size SecondQueue {left=h, right=t} = length h + length t
	
	push _ _ = error "Not implemented"
	eject _ = error "Not implemented"