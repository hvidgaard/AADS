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

$bincolor = '#555599';
$fibcolor = '#995599';
$vebcolor = '#CCCC55';
$rbtcolor = '#55CCCC';

$stdline = 'lines linewidth 2 linecolor rgb';

$random = new Graph;
$random->title = '"Random graph averages"';
$bin_rand = new Plot;
$bin_rand->datafile = "< grep \"bin_random\" $cyc_file";
$bin_rand->datamodifiers = 'using 2:5';
$bin_rand->style = "$stdline '$bincolor'";
$bin_rand->title = 'Binary';
$random->addPlot($bin_rand);

$fib_rand = clone $bin_rand;
$fib_rand->datafile = "< grep \"fib_random\" $cyc_file";
$fib_rand->style = "$stdline '$fibcolor'";
$fib_rand->title = 'Fibonacci';
$random->addPlot($fib_rand);

$veb_rand = clone $bin_rand;
$veb_rand->datafile = "< grep \"veb_random\" $cyc_file";
$veb_rand->style = "$stdline '$vebcolor'";
$veb_rand->title = 'van Emde Boas';
$random->addPlot($veb_rand);
$random->output('graphs/random_averages.png');