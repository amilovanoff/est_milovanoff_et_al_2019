# Folder description
The folder 'outputs' contains the output files and some functions to generate the outputs.

# Files to define the simulations
* simulations.xlsx: 
	* Excel spreadsheets that contain the simulations by function. Each spreadsheet represents a function.
	* "Simulation" is the simulation number
	* "Attribute" is the list of attributes
	* "Type" is the type of attribute: 
		* "attr" is a normal attribute (not keep in the simulation output)
		* "attr_tk" is an attribute to keep in the simulation output
* sens_analysis.xlsx:
	
* scenarios.xlsx:

* simulations_planning.R: Generates all default, simulations and sensitivity analyses results

# Functions to simulate and save, or load and return the default, simulation and sensitivity analyses results

# Other folders
* out_def: Contains the default results in .RData by function
* out_sens_a: Contains the sensitivity analysis results in .Rdata by function
* out_sim: Contains the simulation results in .Rdata by function
* plots: Contains some saved plots 