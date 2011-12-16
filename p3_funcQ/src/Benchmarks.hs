module Benchmarks
( simple
) where
import Queue

simple :: (Queue a) => a -> Int -> a
simple queue size =
	let full = populate queue size
	in snd $ until ((==) Nothing . fst) (remove . snd) (Just 0, full)

--reuseremove :: (Queue a) => a -> Int -> a
--reuseremove queue size =
--	let full = populate queue size
--	in 

-- populate the queue with random numbers
populate :: (Queue a) => a -> Int -> a
populate q 0 = q
populate q s = populate (insert 4 q) (s-1)
-- 4 was thrown with a dice and is completely random