module Queue ( Queue (..) ) where

class Queue a where
	insert :: Int -> a -> a
	remove :: a -> (Maybe Int, a)