module Tests ( sequential ) where
import Queue

sequential :: (Queue a) => a -> IO ()
sequential queue = do
	let full = foldl (flip insert) queue [1..20]
	popIt (remove full)
	where
		popIt (Nothing, _) = putStrLn "list empty"
		popIt (i, q) = do
			putStrLn $ show i
			popIt (remove q)