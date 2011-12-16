{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import System.IO()
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
	start :: Int,
	end :: Int,
	step :: Int,
	logfile :: FilePath
} deriving (Show, Data, Typeable)

bm :: Benchmark
bm = Benchmark {
	queue = def &= help "The queue to benchmark" &= opt "first",
	benchmark = def &= help "The benchmark to use" &= opt "simple",
	start = def &= help "The start size of the queue" &= opt (5000 :: Int),
	end = def &= help "The end size of the queue" &= opt (50000 :: Int),
	step = def &= help "The increment per iteration" &= opt (1000 :: Int),
	logfile = def &= help "Where to log to"
}

main :: IO ()
main = do
	Benchmark{..} <- cmdArgs bm
	run queue benchmark start end step logfile

run :: String -> String -> Int -> Int -> Int -> FilePath -> IO()
run _ _ size end _ _
	| size > end = return ()
run queue@"first" benchmark size end step logfile = do
	time <- runBenchmark No1.makelist (getBenchmark benchmark) size
	logPerformance logfile (queue, benchmark, size, time)
	run queue benchmark (size+step) end step logfile
run queue@"second" benchmark size end step logfile = do
	time <- runBenchmark No2.makelist (getBenchmark benchmark) size
	logPerformance logfile (queue, benchmark, size, time)
	run queue benchmark (size+step) end step logfile
run queue@"third" benchmark size end step logfile = do
	time <- runBenchmark No3.makelist (getBenchmark benchmark) size
	logPerformance logfile (queue, benchmark, size, time)
	run queue benchmark (size+step) end step logfile
run queue@"fourth" benchmark size end step logfile = do
	time <- runBenchmark No4.makelist (getBenchmark benchmark) size
	logPerformance logfile (queue, benchmark, size, time)
	run queue benchmark (size+step) end step logfile
run _ benchmark _ _ _ _ = error ("unknown benchmark: " ++ benchmark)

getBenchmark :: (Queue a) => String -> (a -> Int -> a)
getBenchmark "simple" = B.simple
getBenchmark "insertonly" = B.insertonly
getBenchmark "reuseremove_snd" = B.reuseremove_snd
getBenchmark "reuseremove_fth" = B.reuseremove_fth
getBenchmark _ = error "unknown benchmark"

runBenchmark :: (Queue a) => a -> (a -> Int -> a) -> Int -> IO(Integer)
runBenchmark q bench_fn s = do
	start <- getCPUTime
	_ <- return $! bench_fn q s
	stop <- getCPUTime
	print ((stop-start) `div` 1000000)
	return ((stop-start) `div` 1000000)

logPerformance :: FilePath -> (String, String, Int, Integer) -> IO()
logPerformance file (q, b, s, time) = do
	--print ("Writing to " ++ (show file))
	appendFile file ("\n"++q++"\t"++b++"\t"++(show s)++"\t"++(show time))


