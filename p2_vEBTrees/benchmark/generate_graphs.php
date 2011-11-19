#!/usr/bin/php
<?php
require_once 'gnuplot/Graph.php';
require_once 'gnuplot/Plot.php';
Graph::setDefaults(array(
	'terminal' => 'png #FFFFFF nocrop enhanced font helvetica 18 size 1200,900',
	'grid' => null,
	'label' => '"Tree size"',
	'xlabel' => '"vertices"',
	'ylabel' => '"Time (ms)"',
	'key' => 'bmargin'
));

$cyc_file = 'logs/concatenated.cyc.averages';
$abs_file = 'logs/concatenated.abs.averages';

$binary = new stdClass;
$binary->name = 'Binary';
$binary->color = '#555599';
$binary->selector = 'bin';

$fibonacci = new stdClass;
$fibonacci->name = 'Fibonacci';
$fibonacci->color = '#995599';
$fibonacci->selector = 'fib';

$veb = new stdClass;
$veb->name = 'van Emde Boas';
$veb->color = '#CCCC55';
$veb->selector = 'veb';

$redblack = new stdClass;
$redblack->name = 'Red Black';
$redblack->color = '#55CCCC';
$redblack->selector = 'rbt';

$stdline = 'lines linewidth 2 linecolor rgb';

$columns = array(3 => 'minimum', 4 => 'maximum', 5 => 'averages', 6 => 'standard deviation', 7 => 'samples');
$generators = array('random' => 'Random', 'dkmax' => 'Decrease Key maximized', 'dkmax2' => 'Decrease Key maximized v2');
$algorithms = array($binary, $fibonacci, $veb, $redblack);
foreach($columns as $column => $columnName) {
	foreach($generators as $generator => $generatorName)  {
		$graph = new Graph;
		$graph->title = "\"$generatorName graph $columnName\"";
		foreach($algorithms as $algo) {
			$plot = new Plot;
			$plot->datafile = "< grep \"{$algo->selector}_{$generator}\" $cyc_file";
			$plot->datamodifiers = "using 2:$column";
			$plot->style = "$stdline '$algo->color'";
			$plot->title = $algo->name;
			$graph->addPlot($plot);
		}
		$graph->output("graphs/{$generator}_{$columnName}.png");
	}
}