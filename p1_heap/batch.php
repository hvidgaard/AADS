#!/usr/bin/php
<?php
echo "Running batch jobs\n";
$graphs = array('random', 'dkmax', 'dkmax2');
$queues = array('bin', 'fib', 'pq');
$iterations = 2;
$vertices = pow(2, 13)+pow(2, 14)+1;
foreach($graphs as $graph) {
	if($graph == 'random')
		$seed = rand();
	foreach($queues as $queue) {
		for($i = 0; $i < $iterations; $i++) {
			if($graph == 'random')
				$run = "./test $graph $queue $vertices $seed";
			else
				$run = "./test $graph $queue $vertices";
			echo $run."\n";
			echo "At graph $graph on queue $queue with $vertices vertices.\n";
			echo `$run`;
		}
	}
}
