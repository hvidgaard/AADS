module First
( FirstQueue (..)
, makelist ) where
import Queue

data FirstQueue = FirstQueue [Int] deriving (Show)

makelist :: FirstQueue
makelist = FirstQueue []

instance Queue FirstQueue where
	insert e (FirstQueue xs) = FirstQueue (xs ++ [e])
	remove (FirstQueue []) = ( Nothing, FirstQueue [] )
	remove (FirstQueue (x:xs)) = ( Just x, FirstQueue xs )