#!/usr/bin/php
<?php
for($i = 0; file_exists($i.'.test'); $i++);
$filename = $i;

$numberOfVertices = 10;
// The chance an edge exists between two vertices, in percent
$chanceOfEdge = 15;
// The maximum weight an edge can have
$maxWeight = 20;

$testHandle = fopen($filename.'.test', 'w+');
$dotHandle = popen("dot -Tpng -o$filename.png", 'w');

$source = rand(0, $numberOfVertices);

fputs($testHandle, "$numberOfVertices\n");
fputs($testHandle, "$source\n");

fputs($dotHandle, "digraph G {\n");
fputs($dotHandle, "\t$source [color=red]\n");

for($i = 0; $i < $numberOfVertices; $i++) {
	for($j = 0; $j < $numberOfVertices; $j++) {
		if($j == $i+1 || ($i != $j && rand(0, 99) < $chanceOfEdge)) {
			$weight = rand(1, $maxWeight);
			fputs($testHandle, $weight);
			fputs($dotHandle, "\t$i -> $j [label=\"$weight\"]\n");
		} else {
			fputs($testHandle, '0');
		}
		if($j < $numberOfVertices - 1)
			fputs($testHandle, ' ');
	}
	fputs($testHandle, "\n");
}
fputs($dotHandle, "}");

fclose($testHandle);
fclose($dotHandle);

print("$filename.test and $filename.dot have been generated\n");