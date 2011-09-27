#!/usr/bin/php
<?php
echo "Running batch jobs\n";
$graphs = array('random', 'dkmax');
$queues = array('bin', 'fib', 'pq');
$iterations = 128;
for($exp = 11; $exp < 12; $exp++) {
	$vertices = pow(2, $exp)+1;
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
				echo `$run`;
			}
		}
	}
}