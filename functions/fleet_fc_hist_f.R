###>Function: Creates the historical fuel consumption of all vehicle types
fleet_fc_hist_f <- function(){
  #Assign arguments default values
  source("architecture/attribute_f.R",local = TRUE)
  attribute_f("fleet_fc_hist_f")
  #Functions
  source("functions/fc_hist_f.R",local = TRUE)
  #Input files
  vehicle_technology <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  vehicle_size<-c("Car","Light truck")
  if (exists("hist_fc_dt",inherits = FALSE)){remove(hist_fc_dt)}
  for (techno in unique(vehicle_technology$own)){
    for (size in vehicle_size){
      fuel_l<-unique(unlist(strsplit(vehicle_technology$`Fuel type`[which(vehicle_technology$own==techno)][1],";")))
      for (fuel_type in fuel_l){
        FC_dt<-do.call(fc_hist_f,list(size=size,techno=techno,fuel_type=fuel_type))
        if (!exists("hist_fc_dt")){
          hist_fc_dt<-FC_dt
        } else {
          hist_fc_dt<-rbind(hist_fc_dt, FC_dt)
        }
      }
    }
  }
  return(list(fleet_fc_hist=hist_fc_dt))
}
