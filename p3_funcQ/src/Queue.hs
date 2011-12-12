module Queue
( push
, pop
, inject
, eject
, size
, Queue
) where

class Queue a where
	push :: Int -> a -> a
	pop :: a -> (Maybe Int, a)
	inject :: Int -> a -> a
	eject :: a -> (Maybe Int, a)
	size :: a -> Int
