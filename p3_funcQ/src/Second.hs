{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Second
( SecondQueue (..)
, makelist ) where
import Queue

-- Paired lists
data SecondQueue = SecondQueue {left :: [Int], right :: [Int]} deriving (Show)

makelist :: SecondQueue
makelist = SecondQueue {left=[], right=[]}

instance Queue SecondQueue where
	insert y SecondQueue {..} = ( SecondQueue left (y:right) )
	remove (SecondQueue [] []) = ( Nothing, (SecondQueue [] []))
	remove (SecondQueue [] right) = remove (SecondQueue (reverse right) [])
	remove (SecondQueue (x:xs) right) = ( Just x, SecondQueue {left=xs, right} )