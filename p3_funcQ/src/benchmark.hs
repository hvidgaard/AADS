--import System.Environment
--import Data.List
--import Queue
import Simple as SQ
import Second as NDQ
import Third as RDQ
import Tests as T

main :: IO ()
main = do
	--(fn:q:args) <- getArgs
	T.sequential $ SQ.makelist 0
	T.sequential $ NDQ.makelist 0
	T.sequential $ RDQ.makelist 0