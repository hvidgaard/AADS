module Second
( Second (..)
, makelist ) where
import Queue

-- Paired lists
data SecondQueue = SecondQueue {left :: [Int], right :: [Int]} deriving (Show)

makelist :: SecondQueue
makelist = SecondQueue {left=[], right=[]}

instance Queue SecondQueue where
	insert y SecondQueue {left=h, right=t} = ( SecondQueue {left=h, right=y:t} )
	remove SecondQueue {left=[], right=[]} = ( Nothing, SecondQueue {left=[], right=[]})
	remove SecondQueue {left=[], right=t} = remove SecondQueue {left=reverse t, right=[]}
	remove SecondQueue {left=x:xs, right=t} = ( Just x, SecondQueue {left=xs, right=t} )