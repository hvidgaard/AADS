#!/usr/bin/php
<?php
$term = false;

if($argc == 2) {
	$argv[2] = $argv[1].'.averages';
	echo "No outfile specified, assuming $argv[2]\n";
	$term = true;
	$argc++;
}
$argv[1] = str_replace(getcwd().'/', '', $argv[1]);
$argv[2] = str_replace(getcwd().'/', '', $argv[2]);

if($argc != 3) {
	echo "Usage\n\taverages infile outfile\n";
	exit(1);
}
if(!file_exists($argv[1])) {
	echo "$argv[1] does not exist\n";
	exit(2);
}
echo "Parsing $argv[1]\n";
if(file_exists($argv[2]) && filemtime($argv[2]) > filemtime($argv[1])) {
	echo "outfile newer than infile. Skipping it.\n";
	exit(0);
}

$infile = fopen($argv[1], 'r');
if(!$infile) {
	echo "$argv[1] could not be opened for reading.\n";
	exit(4);
}

$outfile = fopen($argv[2], 'w+');
if(!$outfile) {
	echo "$argv[2] could not be opened for writing.\n";
	exit(5);
}

$i = 1;
list($lines, ) = explode(' ', trim(`wc -l $argv[1]`));
$groups = array();
$errors = 0;
while($line = fgets($infile)) {
	$line = trim($line);
	if(empty($line))
		continue;
	if(!preg_match('/^([a-z_]+)\t([a-z_]+)\t([0-9]+)\t([0-9]+\.[0-9]+)\t([0-9]+\.[0-9]+)$/i', $line, $matches)) {
		echo "Could not parse line $i of file $argv[1]\n";
		echo "\tThe line is $line\n";
		$errors++;
		continue;
	}
	$group = $matches[1].'_'.$matches[2];
	$size = $matches[3];
	$running_time = floatval($matches[4]);
	if(!array_key_exists($group, $groups))
		$groups[$group] = array();
	if(!array_key_exists($size, $groups[$group]))
		$groups[$group][$size] = array();
	$groups[$group][$size][] = $running_time;
	if($i % 100 == 0 && $term)
		echo "\rAt line $i of $lines";
	$i++;
}
$i--;
if($term)
	echo "\rAt line $i of $lines\n";
fclose($infile);

if($errors)
	echo "$errors errors while parsing.";

fputs($outfile, "Group\tSize\tMinimum\tMaximum\tAverage\tStandard deviation\tSamples\n");
foreach($groups as $group => $sizes) {
	if($term)
		echo "Parsing group $group\n";
	foreach($sizes as $size => $running_times) {
		$samples = 0;
		foreach($running_times as $running_time) {
			if($samples == 0) {
				$sum = $min = $max = $running_time;
			} else {
				$min = min($min, $running_time);
				$max = max($max, $running_time);
				$sum += $running_time;
			}
			$samples++;
		}
		$avg = $sum/$samples;
		$deviation_sum = 0;
		foreach($running_times as $running_time)
			$deviation_sum += pow($avg - $running_time, 2);
		$deviation = 0;
		if($samples > 1)
			$deviation = sqrt($deviation_sum / ($samples - 1));
		fputs($outfile, "$group\t$size\t$min\t$max\t$avg\t$deviation\t$samples\n");
	}
}
fclose($outfile);
echo "Done.\n";




