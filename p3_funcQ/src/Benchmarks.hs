module Benchmarks
( simple
) where
import Queue

simple :: (Queue a) => a -> Int -> a
simple queue size =
	let full = populate queue size
	in snd $ until ((==) Nothing . fst) (remove . snd) (Just 0, full)
	where
		-- populate the queue with random numbers
		populate q 0 = q
		populate q s = populate (insert 4 q) (s-1)
		-- 4 was thrown with a dice and is completely random
