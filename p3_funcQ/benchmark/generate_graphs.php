#!/usr/bin/php
<?php
require_once 'gnuplot/Graph.php';
require_once 'gnuplot/Plot.php';
Graph::setDefaults(array(
	'terminal' => 'png #FFFFFF nocrop enhanced font helvetica 18 size 1200,900',
	'grid' => null,
	'xlabel' => '"Vertices"',
	'ylabel' => '"Time (ms)"',
	'key' => 'bmargin'
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

$generators = array("simple" => 'Populate/Clear', 'insertonly' => 'Inject Only', 'reuseremove_snd' => 'Queue Reuse (#1)', 'reuseremove_fth' => 'Queue Reuse (#2)');
$algorithms = array($simple, $lrpair, $pairs, $preev);
$ranges = array('full' => null, 'low' => '[1:200]', 'mid' => '[1:5000]');
foreach($ranges as $rangeName => $range) {
		foreach($generators as $generator => $generatorName)  {
			
			$png = "graphs/{$generator}_{$rangeName}.png";
			$eps = "graphs/{$generator}_{$rangeName}.eps";
			if(file_exists($png) && file_exists($eps))
				if(filemtime($file) < filemtime($png) && filemtime(__FILE__) < filemtime($png))
					continue;
			
			$graph = new Graph;
			if($range)
				$graph->xrange = $range;
			
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
}