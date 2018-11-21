# Folder description
This folder contains some files that describe the model architecture and some files that are essential to run the simulations.

# Files description

## Files to update manually
* attribute_value.csv: List of all exogenous function's attributes with their abbreviated name (Attribute), their type (Type, i.e., 'cha' means character, 'num' means numeric), their value to be used in the current simulations (Value), their default value (Default), all working values (All) and their description (Description).  

## Scripts or functions
* attribute_f.R: Contains a function that is used in each model's function to attribute the values of the exogenous attributes from attribute_value.csv. To work properly, it needs attribute_value.csv, and function_attributes.csv.
* function_attribute_builder.R: Creates a .csv file with all the model's functions (scripts finishing with '_f.R' in the function folder), all their attributes (exogenous and endogenous) and the default values taken from the function's script.
* function_descriptions.R: Creates a .csv file with the descriptions of model's functions.
* function_inputs.R: Creates a .csv file with the inputs of the model's functions (exogenous or indogenous to the model).
* library_dependencies.R: List and install the set of libraries used at some point in the model.

## Files automatically generated
from function_attribute_builder.R
* function_attributes.csv: Contains the list of model's functions and their attributes by type ('Implicit' for endogenous attributes - outputs of other model's functions; 'Explicit' for exogenous attributes and 'Run' for attributes only associated with simulation parameters).
* function_explicit_attributes.csv: Contains the list of model's functions with the exogenous attributes.

from function_descriptions.R
* functions_descriptions.csv: Contains the list of model's functions and their functional descriptions.

from function_inputs.R
* function_inputs.csv: Contains the list of model's functions and their inputs by type (.csv, .xlsx, .R).
* function_inputs_matrix.csv: Contains the same information than function_inputs.csv but in matrix format (with the rows beeing the inputs and the columns the model's functions).
* function_sources_matrix.csv: Contains the same information than function_inputs_matrix.csv but only for model's function as inputs.
