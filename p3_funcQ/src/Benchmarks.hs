module Benchmarks
( simple
, alternate
, reuseremove_snd
, reuseremove_fth
) where
import Queue

simple :: (Queue a) => a -> Int -> a
simple queue size =
	let full = populate queue size
	in snd $ until ((==) Nothing . fst) (remove . snd) (Just 0, full)

alternate :: (Queue a) => a -> Int -> a
alternate queue 0 = queue
alternate queue size =
	let full = insert 4 (snd . remove $ queue)
	in seq (alternate queue (size-1)) (snd . remove $ insert 4 full)

reuseremove_snd :: (Queue a) => a -> Int -> a
reuseremove_snd queue size =
	let full = populate queue size
	in repeatR full size

reuseremove_fth :: (Queue a) => Int -> a -> Int -> a
reuseremove_fth 0 queue size = queue
reuseremove_fth repeats queue size =
	let full = populate queue size
	in seq (reuseremove_fth (repeats-1) queue size) (snd . remove $ populate full (diff size))
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
repeatR queue repeats = seq (remove . snd . remove $ insert 4 queue) (repeatR queue (repeats-1))
