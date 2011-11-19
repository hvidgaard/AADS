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

$generators = array('random' => 'Random', 'dkmax' => 'Decrease Key maximized', 'dkmax2' => 'Decrease Key maximized v2');
$algorithms = array($binary, $fibonacci, $veb, $redblack);
foreach($generators as $generator => $generatorName)  {
	$graph = new Graph;
	$graph->title = "\"$generatorName graph averages\"";
	foreach($algorithms as $algo) {
		$plot = new Plot;
		$plot->datafile = "< grep \"{$algo->selector}_{$generator}\" $cyc_file";
		$plot->datamodifiers = 'using 2:5';
		$plot->style = "$stdline '$algo->color'";
		$plot->title = $algo->name;
		$graph->addPlot($plot);
	}
	$graph->output("graphs/{$generator}_averages.png");
}
