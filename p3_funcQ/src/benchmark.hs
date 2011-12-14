--import System.Environment
--import Data.List
--import Queue
import First as No1
import Second as No2
import Third as No3
import Fourth as No4
import Tests as T

main :: IO ()
main = do
	--(fn:q:args) <- getArgs
	T.sequential $ No1.makelist
	print "1 done"
	T.sequential $ No2.makelist
	print "2 done"
	T.sequential $ No3.makelist
	print "3 done"
	T.sequential $ No4.makelist
	print "4 done"