#!/usr/bin/php
<?php
if($argc != 2) {
	print("Usage ./generator.php [filename]");
	exit(1);
}
if(file_exists($argv[1])) {
	print("The file $argv[1] already exists");
	exit(2);
}

$numberOfVertices = 20;
// The chance an edge exists between two vertices, in percent
$chanceOfEdge = 40;
// The maximum weight an edge can have
$maxWeight = 100;

$handle = fopen($argv[1], 'w+');
fputs($handle, $numberOfVertices."\n");
fputs($handle, rand(0, $numberOfVertices)."\n");
for($i = 0; $i < $numberOfVertices; $i++) {
	for($j = 0; $j < $numberOfVertices; $j++) {
		if($i != $j && rand(0, 99) < $chanceOfEdge)
			fputs($handle, rand(1, $maxWeight));
		else
			fputs($handle, '0');
		if($j < $numberOfVertices - 1)
			fputs($handle, ' ');
	}
	fputs($handle, "\n");
}









