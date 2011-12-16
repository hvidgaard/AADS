<?php
class Graph {
	
	private static $defaults = array();
	public static function setDefaults(array $defaults) {
		self::$defaults = $defaults;
	}
	
	public function __construct() {
		$this->options = self::$defaults;
	}
	
	private $plots = array();
	public function addPlot(Plot $plot) {
		$this->plots[] = $plot;
	}
	
	public function __clone() {
		
	}
	
	private $options;
	public function __set($name, $value) {
		$this->options[$name] = $value;
	}
	
	public function __get($name) {
		if(array_key_exists($name, $this->options))
			return $this->options[$name];
	}
	
	public function __unset($name) {
		unset($this->options[$name]);
	}
	
	public function __toString() {
		$script = '';
		foreach($this->options as $name => $value) {
			if($value !== null)
				$script .= "set $name $value\n";
			else
				$script .= "set $name\n";
		}
		$script .= "plot \\\n".implode(", \\\n", $this->plots)."\n";
		return $script;
	}
	
	public function output($file) {
		$oldOutput = $this->output;
		$this->output = "'$file'";
		$handle = popen("gnuplot", "w");
		fputs($handle, $this);
		pclose($handle);
		$this->output = $oldOutput;
	}
}