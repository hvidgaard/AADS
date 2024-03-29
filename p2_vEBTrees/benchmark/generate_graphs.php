#!/usr/bin/php
<?php
require_once 'gnuplot/Graph.php';
require_once 'gnuplot/Plot.php';
Graph::setDefaults(array(
	'terminal' => 'png #FFFFFF nocrop enhanced font helvetica 18 size 1200,900',
	'grid' => null,
	'xrange' => '[0:12000]',
	'xlabel' => '"Vertices"',
	'ylabel' => '"Time (ms)"',
	'key' => 'bmargin'
));

$cyc_file = 'logs/concatenated.cyc.averages';
$abs_file = 'logs/concatenated.abs.averages';

$binary = new stdClass;
$binary->name = 'Binary';
$binary->color = '#6c71c4';
$binary->selector = 'bin';

$fibonacci = new stdClass;
$fibonacci->name = 'Fibonacci';
$fibonacci->color = '#002b36';
$fibonacci->selector = 'fib';

$veb = new stdClass;
$veb->name = 'van Emde Boas';
$veb->color = '#b58900';
$veb->selector = 'veb';

$redblack = new stdClass;
$redblack->name = 'Red Black';
$redblack->color = '#dc322f';
$redblack->selector = 'rb';

$stdline = 'lines linewidth 2 linecolor rgb';

$measurements = array('cycles' => 'logs/concatenated.cyc.averages', 'absolute' => 'logs/concatenated.abs.averages');
$columns = array(3 => 'minimum', 4 => 'maximum', 5 => 'averages', 6 => 'standard_deviation', 7 => 'samples');
$generators = array("random" => 'Random', /*'dkmax' => 'Decrease Key maximized', */'dkmax2' => 'Decrease Key maximized v2', 'random_list' => 'Random List');
$algorithms = array($binary, $fibonacci, $veb);

foreach($measurements as $measurement => $file) {
	foreach($columns as $column => $columnName) {
		foreach($generators as $generator => $generatorName)  {
			
			$png = "graphs/{$generator}_{$columnName}_{$measurement}.png";
			$eps = "graphs/{$generator}_{$columnName}_{$measurement}.eps";
			if(file_exists($png) && file_exists($eps))
				if(filemtime($file) < filemtime($png) && filemtime(__FILE__) < filemtime($png))
					continue;
			$columnName = str_replace('_', ' ', $columnName);
			
			$graph = new Graph;
			
			if($columnName == 'samples') {
				$graph->ylabel = '"Samples"';
			} else {
				if($measurement == 'cycles')
					$graph->title = "\"$generatorName graph $columnName \\n(Clock cycles)\"";
				else
					$graph->title = "\"$generatorName graph $columnName \\n(Abs. time)\"";
			}
			
			
			$_algorithms = $algorithms;
			if($generator == 'random_list') {
				$_algorithms = array($veb, $redblack);
				$graph->xrange = '[0:112000]';
			}
			$match = $generator;
			if($generator == 'random')
				$match = "random\t";
				
			foreach($_algorithms as $algo) {
				$plot = new Plot;
				$plot->datafile = "< grep \"{$algo->selector}_{$match}\" $file";
				$plot->datamodifiers = "using 2:$column";
				$plot->style = "$stdline '$algo->color'";
				$plot->title = $algo->name;
				$graph->addPlot($plot);
			}
//			$graph->output($png);
//			$graph->terminal = "epslatex linewidth 3";
//			$graph->output($eps);
			echo $graph;
		}
	}
}