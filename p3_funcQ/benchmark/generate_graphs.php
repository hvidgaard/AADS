#!/usr/bin/php
<?php
require_once 'gnuplot/Graph.php';
require_once 'gnuplot/Plot.php';
Graph::setDefaults(array(
	'terminal' => 'png #FFFFFF nocrop enhanced font helvetica 18 size 1200,900',
	'grid' => null,
	'xlabel' => '"Vertices"',
	'ylabel' => '"Time (ms)"',
	'key' => 'bmargin',
	'xrange' => '[0:20000]'
));

$file = 'logs/concatenated.averages';

$simple = new stdClass;
$simple->name = 'Simple List';
$simple->color = '#6c71c4';
$simple->selector = 'first';

$lrpair = new stdClass;
$lrpair->name = 'Left-Right pair';
$lrpair->color = '#002b36';
$lrpair->selector = 'second';

$pairs = new stdClass;
$pairs->name = 'Paired lazy lists';
$pairs->color = '#b58900';
$pairs->selector = 'fourth';

$preev = new stdClass;
$preev->name = 'Pre-evaluated lists';
$preev->color = '#dc322f';
$preev->selector = 'third';

$stdline = 'lines linewidth 2 linecolor rgb';

$generators = array(
	'simple' => 'Populate/Clear',
	'reuseremove_snd' => 'Queue Reuse (#2)',
	'reuseremove_fth' => 'Queue Reuse (#1)');
$algorithms = array(
	$simple,
	$lrpair,
	$pairs,
	$preev);
foreach($generators as $generator => $generatorName)  {
	
	$png = "graphs/{$generator}.png";
	$eps = "graphs/{$generator}.eps";
	
	if(file_exists($png) && file_exists($eps))
		if(filemtime($file) < filemtime($png) && filemtime(__FILE__) < filemtime($png))
			continue;
	
	$graph = new Graph;
	
	$graph->title = "\"$generatorName benchmark\"";
	foreach($algorithms as $algo) {
		$plot = new Plot;
		$plot->datafile = "< grep \"{$algo->selector}_{$generator}\" $file";
		$plot->datamodifiers = "using 2:5";
		$plot->style = "$stdline '$algo->color'";
		$plot->title = $algo->name;
		$graph->addPlot($plot);
	}
	$graph->output($png);
	$graph->terminal = "epslatex linewidth 3";
	$graph->output($eps);
	// echo $graph;
}

$generators = array('simple' => 'Populate/Clear (high)');
$algorithms = array($lrpair, $pairs, $preev);
foreach($generators as $generator => $generatorName)  {
	
	$png = "graphs/{$generator}_high.png";
	$eps = "graphs/{$generator}_high.eps";
	
	if(file_exists($png) && file_exists($eps))
		if(filemtime($file) < filemtime($png) && filemtime(__FILE__) < filemtime($png))
			continue;
	
	$graph = new Graph;
	$graph->xrange = '[0:2000000]';
	$graph->title = "\"$generatorName benchmark\"";
	foreach($algorithms as $algo) {
		$plot = new Plot;
		$plot->datafile = "< grep \"{$algo->selector}_{$generator}\" $file";
		$plot->datamodifiers = "using 2:5";
		$plot->style = "$stdline '$algo->color'";
		$plot->title = $algo->name;
		$graph->addPlot($plot);
	}
	$graph->output($png);
	$graph->terminal = "epslatex linewidth 3";
	$graph->output($eps);
	// echo $graph;
}