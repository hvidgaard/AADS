module Benchmarks
( simple
, insertonly
, reuseremove_snd
, reuseremove_fth
) where
import Queue

simple :: (Queue a) => a -> Int -> a
simple queue size =
	let full = populate queue size
	in snd $ until ((==) Nothing . fst) (remove . snd) (Just 0, full)

insertonly :: (Queue a) => a -> Int -> a
insertonly queue size =
	let full = populate queue size
	in seq full queue

reuseremove_snd :: (Queue a) => a -> Int -> a
reuseremove_snd queue size =
	let full = populate queue size
	in repeatR full size

reuseremove_fth :: (Queue a) => a -> Int -> a
reuseremove_fth queue size =
	let full = populate queue size
	in repeatR (takeQ full (diff size)) size
	where
		diff :: Int -> Int
		diff elms =
			let e = fromIntegral elms
			in round (2^(ceiling $ sqrt e) - e - 2)

-- populate the queue with random numbers
populate :: (Queue a) => a -> Int -> a
populate q 0 = q
populate q s = populate (insert 4 q) (s-1)
-- 4 was thrown with a dice and is completely random

takeQ :: (Queue a) => a -> Int -> a
takeQ queue 0 = queue
takeQ queue amount = takeQ (snd $ remove queue) (amount-1)

repeatR :: (Queue a) => a -> Int -> a
repeatR queue 0 = queue
repeatR queue repeats = seq (remove queue) (repeatR queue (repeats-1))