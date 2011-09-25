#!/usr/bin/php
<?php
if($argc < 2) {
	print("Usage: generator.php vertices [chanceOfEdge] [maxWeight]\n");
	print("                              (default: 15)  (default:20)\n");
	exit(1);
}
for($i = 0; file_exists(__DIR__.'/'.$i.'.test'); $i++);
$testFile = __DIR__.'/'.$i.'.test';
$dotFile = __DIR__.'/'.$i.'.png';

$useDot = true;

$numVertices = $argv[1];
// The chance an edge exists between two vertices, in percent
$chanceOfEdge = @$argv[2]?:15;
// The maximum weight an edge can have
$maxWeight = @$argv[3]?:20;

$source = rand(0, $numVertices);

$testHandle = fopen($testFile, 'w+');
fputs($testHandle, "$numVertices\n");
fputs($testHandle, "$source\n");

if($useDot) {
	$dotHandle = popen("dot -Tpng -o$dotFile", 'w');
	fputs($dotHandle, "digraph G {\n");
	fputs($dotHandle, "\t$source [color=red]\n");
}

for($i = 0; $i < $numVertices; $i++) {
	echo "\rProgress: ".(round($i/$numVertices, 2)*100)."%";
	$line = '';
	for($j = 0; $j < $numVertices; $j++) {
		if($j == $i+1 || ($i != $j && rand(0, 99) < $chanceOfEdge)) {
			$weight = rand(1, $maxWeight);
			$line .= $weight;
			if($useDot)
				fputs($dotHandle, "\t$i -> $j [label=\"$weight\"]\n");
		} else {
			$line .= 0;
		}
		if($j < $numVertices - 1)
			$line .= ' ';
	}
	fputs($testHandle, $line."\n");
}
echo "\n";
if($useDot) {
	fputs($dotHandle, "}");
	fclose($dotHandle);
}

fclose($testHandle);

print("$testFile and $dotFile have been generated\n");
