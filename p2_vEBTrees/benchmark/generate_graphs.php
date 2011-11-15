#!/usr/bin/php
<?php
require_once 'gnuplot/Graph.php';
require_once 'gnuplot/Plot.php';
Graph::setDefaults(array(
	'terminal' => 'png #FFFFFF nocrop enhanced font helvetica 18 size 1200,900',
	'grid' => null,
	'label' => '"Tree size"'
));


$graph = new Graph;
$plot1 = new Plot;
$plot1->datafile = '< grep veb_trees logs/concatenated.averages';
$plot1->datamodifiers = 'using 2:($5/1000)';
$plot1->style = "lines linewidth 2 linecolor rgb '#555599'";
$plot1->title = 'Some graph';
$graph->addPlot($plot1);
echo $graph;