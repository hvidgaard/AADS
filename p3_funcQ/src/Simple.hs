module Simple
( push
, pop
, inject
, eject
, size
, makelist
) where
import Queue

data SimpleQueue = SimpleQueue [Int] deriving (Show)

makelist :: Int -> SimpleQueue
makelist x = SimpleQueue [x]

instance Queue SimpleQueue where
	inject y (SimpleQueue xs) = SimpleQueue (xs ++ [y])
	pop (SimpleQueue []) = ( Nothing, SimpleQueue [] )
	pop (SimpleQueue (x:xs)) = ( Just x, SimpleQueue xs )
	
	size (SimpleQueue xs) = length xs
	
	push y (SimpleQueue xs) = SimpleQueue (y:xs)
	eject (SimpleQueue []) = ( Nothing, SimpleQueue [] )
	eject (SimpleQueue xs) = ( Just (last xs), SimpleQueue (init xs) )