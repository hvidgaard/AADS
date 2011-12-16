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
	in repeatR (populate full (diff size)) size
	where
		diff :: Int -> Int
		diff count =
			let
				elms = fromIntegral count
				elms_exp = max ((ceiling $ logBase 2 elms)+1) 0
			in max (round (2^elms_exp-elms-2)) 0

-- populate the queue with random numbers
populate :: (Queue a) => a -> Int -> a
populate q 0 = q
populate q s = populate (insert 4 q) (s-1)
-- 4 was thrown with a dice and is completely random

repeatR :: (Queue a) => a -> Int -> a
repeatR queue 0 = queue
repeatR queue repeats = seq (remove queue) (repeatR queue (repeats-1))