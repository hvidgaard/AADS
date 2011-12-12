module Tests
( sequential
) where
import Queue

sequential :: (Queue a) => a -> IO ()
sequential queue = do
	let full = foldl (flip inject) queue [1..20]
	popIt full
	where popIt q
		| size q == 0 = putStrLn ""
		| otherwise = do
			let (i, newQ) = pop q
			putStrLn $ show i
			popIt newQ
