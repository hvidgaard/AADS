<?php
class Plot {
	
	public function __construct() {
		
	}
	
	private $ranges;
	private $iteration;
	private $datafile;
	private $datamodifiers;
	private $axes;
	private $title;
	private $style;
	public function __set($name, $value) {
		$this->{$name} = $value;
	}
	
	public function __unset($name) {
		unset($this->{$name});
	}
	
	public function __toString() {
		return
			(isset($this->ranges)?$this->ranges." \\\n":'').
			(isset($this->iteration)?"\t".$this->iteration." \\\n":'').
			"'".$this->datafile."' \\\n\t".$this->datamodifiers." \\\n".
			(isset($this->axes)?"\taxes ".$this->axes." \\\n":'').
			(isset($this->title)?"\ttitle '".$this->title."' \\\n":'').
			(isset($this->style)?"\twith ".$this->style:'');
	}
}