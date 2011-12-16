module Tests ( sequential ) where
import Queue
import First as No1
import Second as No2
import Third as No3
import Fourth as No4

sequential :: IO()
sequential = do
	sequentialTest No1.makelist
	sequentialTest No2.makelist
	sequentialTest No3.makelist
	sequentialTest No4.makelist

sequentialTest :: (Show a, Queue a) => a -> IO ()
sequentialTest queue = do
	let full = foldl (flip insert) queue [1..20]
	putStrLn $ show full
	popIt (remove full)
	where
		popIt (Nothing, _) = putStrLn "list empty"
		popIt (Just i, q) = do
			putStrLn $ show i
			popIt (remove q)