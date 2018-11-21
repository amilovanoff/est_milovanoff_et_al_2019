# Folder description
The 'outputs' folder contains the output files and some functions to generate the outputs.

# Files to define and run the simulations
Excel files that are used to define the values of the function's attributes for the simulations.
* simulations.xlsx: Excel spreadsheets that contain the simulations by function. Each spreadsheet represents a function with the following syntax. Each column in the simulation represents a specific run with defined attributes. All attributes not specified have default values.
	* 'Simulation' column contains the simulation number.
	* 'Attribute' column contains 'name' as the second row of the simulations then the list of attributes to be udpated
	* 'Type' column contains the type of attribute: 
		* 'attr' is a normal attribute (not to keep in the simulation output).
		* 'attr_tk' is an attribute to keep in the simulation output.
	* The other columns define the values of the attributes by run in the simulation. The first row of the simulation defined the name of the run, then the other rows are assoociated with the attributes and define the attribute values to use.
* sens_analysis.xlsx: Excel spreadsheets that contain the list of attributes to simulate by sensitivity analysis. For a given sensitivity analysis, the specified function is run with all the values of the specified attributes (from 'architecture/attribute_value.csv', column 'All') considering default values for other attributes. Sensitivity analysis are performed by specifying the scenarios on which the functions are run (see below).  
	* 'Sensitivity' column contains the sensitivity number.
	* 'Sensitivity name' column contains the name of the sensitivity.
	* 'Attributes' column contains the list of attributes to perform the sensitivity on.
	
* scenarios.xlsx: Excel spreadsheets that define scenarios with multiple runs. In the model, functions possess one single set of outputs for a given set of inputs. A scenario is defined as multiple set of inputs to generate multiple sets of outputs. The default scenario is the baseline of the simulations and is used in simulating the default values of the functions. Scenarios can be used in the sensitivity analysis to perform sensitivity analysis of attributes on multiple simulations.
	* 'Scenario' columns contains the name of the scenario.
	* 'Attribute' column contains the name of the attributes.
	* 'Type' column contains the type of the attributes.
	* The other columns define the multiple runs with the row beeing the name of the runs and the other rows are associated with the attributes.

* simulations_planning.R: Generates all default, simulations and sensitivity analyses results.

# Functions to simulate and save, or load and return the default, simulation and sensitivity analyses results
* write_f.R: Contains functions to simulate and save simulation results (from default simulations, to sensitivity analysis)
*read_f.R: Contains the functions to read and return the simulation results.

# Other folders
* out_def: Contains the default results in .RData by function
* out_sens_a: Contains the sensitivity analysis results in .Rdata by function
* out_sim: Contains the simulation results in .Rdata by function
* plots: Contains some saved plots 
