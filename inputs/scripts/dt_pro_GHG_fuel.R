#Conversion factors
LHV_gas_blend_BTUperGal<-116090
LHV_base_gas_BTUperGal<-112194
LHV_base_diesel_BTUperGal<-128450
Den_gas_blend_GramperGal<-2819
Cratio_gas_blend<-0.863
conv<-read.csv("data/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
fuel_specs<-read.csv("data/fuel_specs.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
#PTW gasoline EF (kg CO2/L): (330 g per M, 26.1 M per Gal)
330*26.1*conv["gal","1 L"]*10^-3
#WTP gasoline EF (kg CO2/L):14806g CO2 per mmBTU
14806*10^(-3)*10^(-6)*LHV_base_gas_BTUperGal*conv["gal","1 L"]
Den_gas_blend_GramperGal*conv["gal","1 L"]*10^-3
#WTP diesel EF (kg CO2/L):14292 g CO2 per mmBTU
14292*10^(-3)*10^(-6)*LHV_base_diesel_BTUperGal*conv["gal","1 L"]
#PTW diesel EF (kg CO2/L): (330 g per M, 26.1 M per Gal)
280*26.1*conv["gal","1 L"]*10^-3

0.019*conv["MJ","1 BTU"]*as.numeric(fuel_specs["Gasoline","LHV"])*conv["gal","1 L"]
