module Tests ( sequential ) where
import Queue

sequential :: (Show a, Queue a) => a -> IO ()
sequential queue = do
	let full = foldl (flip insert) queue [1..20]
	putStrLn $ show full
	popIt (remove full)
	where
		popIt (Nothing, _) = putStrLn "list empty"
		popIt (Just i, q) = do
			putStrLn $ show i
			popIt (remove q)