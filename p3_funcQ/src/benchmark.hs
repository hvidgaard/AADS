{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import System.CPUTime
import System.Console.CmdArgs
import Queue
import qualified Benchmarks as B
import qualified First as No1
import qualified Second as No2
import qualified Third as No3
import qualified Fourth as No4

data Benchmark = Benchmark {
	queue :: String,
	benchmark :: String,
	size :: Int,
	logfile :: FilePath
} deriving (Show, Data, Typeable)

bm :: Benchmark
bm = Benchmark {
	queue = def &= help "The queue to benchmark" &= opt "first",
	benchmark = def &= help "The benchmark to use" &= opt "simple",
	size = def &= help "The size of the queue" &= opt (50000 :: Int),
	logfile = def &= help "Where to log to"
}

main :: IO ()
main = run =<< cmdArgs bm

run :: Benchmark -> IO()
run Benchmark {queue="first",benchmark=b,size=s,logfile=f} = do
	time <- runBenchmark No1.makelist (getBenchmark b) s
	logPerformance f ("first", b, s, time)
run Benchmark {queue="second",benchmark=b,size=s,logfile=f} = do
	time <- runBenchmark No2.makelist (getBenchmark b) s
	logPerformance f ("second", b, s, time)
run Benchmark {queue="third",benchmark=b,size=s,logfile=f} = do
	time <- runBenchmark No3.makelist (getBenchmark b) s
	logPerformance f ("third", b, s, time)
run Benchmark {queue="fourth",benchmark=b,size=s,logfile=f} = do
	time <- runBenchmark No4.makelist (getBenchmark b) s
	logPerformance f ("fourth", b, s, time)
run Benchmark {queue=_,benchmark=_,size=_} = error "unknown benchmark"

getBenchmark :: (Queue a) => String -> (a -> Int -> a)
getBenchmark "simple" = B.simple
--getBenchmark "reuse-remove" = B.reuseremove
getBenchmark _ = error "unknown benchmark"

runBenchmark :: (Queue a) => a -> (a -> Int -> a) -> Int -> IO(Integer)
runBenchmark q bench_fn s = do
	start <- getCPUTime
	return $! bench_fn q s
	stop <- getCPUTime
	return (stop-start)

logPerformance :: FilePath -> (String, String, Int, Integer) -> IO()
logPerformance file (q, b, s, time) = do
	print ("Writing to " ++ (show file))
	appendFile file ("\n"++q++"\t"++b++"\t"++(show s)++"\t"++(show time))


