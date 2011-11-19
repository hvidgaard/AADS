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

$file = 'logs/concatenated.averages';

$bincolor = '#555599';
$fibcolor = '#995599';
$vebcolor = '#995599';
$rbtcolor = '#CCCC55';

$stdline = 'lines linewidth 2 linecolor rgb';

$graph = new Graph;
$graph->title = '"Random graph averages"';
$bin_rand = new Plot;
$bin_rand->datafile = "< grep \"bin_random\" $file";
$bin_rand->datamodifiers = 'using 2:5';
$bin_rand->style = "$stdline '$bincolor'";
$bin_rand->title = 'Binary';
$graph->addPlot($bin_rand);

$fib_rand = clone $bin_rand;
$fib_rand->datafile = "< grep \"fib_random\" $file";
$fib_rand->style = "$stdline '$fibcolor'";
$fib_rand->title = 'Fibonacci';
$graph->addPlot($fib_rand);

$graph->output('graphs/random_graph_averages.png');