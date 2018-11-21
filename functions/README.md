# Folder description
This folder contains the function's scripts of the model. The functions contained in 'functions/plots' are mostly used for final plotting.

# Function script description
* Each script is a function. The names have to be the same between script name and function name. Otherwise, the model will not work properly.
* The description of the scipt or function is preceded by '###>'
* Each function has attributes:
	* Endogenous attributes (i.e., outputs of another function used as inputs) and they do no have default values;
	* Exogenous attributes (i.e., attributes defined by the user in 'architecture/attribute_value.csv' for the simulations) marked with the NA default value in the function;
	* Run attributes only associated with simulation parameters (i.e., such as fast_mode which indicates if default inputs should be used to fasten the simulations).
* To initiate the function, all the attributes need to be specified. 
	* The line 'source("architecture/attribute_f.R",local = TRUE)' is read to upload the function 'attribute_f';
	* Then, the function 'attribute_f' is simulated with the name of the model's function. This forces the values of the exogenous attributes, which are not already specified, from 'architecture/attribute_value.csv'.
