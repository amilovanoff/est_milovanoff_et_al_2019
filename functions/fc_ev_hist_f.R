###>Function: Creates the fuel consumption of the EV technology in 2015
fc_ev_hist_f <- function(size,
                      techno,
                      fuel_type,
                      wgt_scen_GREET=NA,
                      fc_ev_mdl=NA){
  #Assign arguments default values
  source("architecture/attribute_f.R",local = TRUE)
  attribute_f("fc_ev_hist_f")
  #Inputs
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  fc_ev_hist_dt<-read.csv("inputs/fc_ev_hist.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other parameters
  #Define the range
  range <- subset(vh_techno,own==techno)$Range
  #Size_GREET:Model size to use in GREET
  if (size=="Light truck" & wgt_scen_GREET%in%c(1,4) |
      size=="Car" & wgt_scen_GREET%in%c(3,4)){ size_GREET = "SUV"
  }else if (size=="Light truck" & wgt_scen_GREET%in%c(2,3)) { size_GREET = "PUT"
  }else { size_GREET="Car"}
  #tmp_techno is the technology name without range
  tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
    
  #In the default case, consider sales-weighted average or GREET data if unavailable
  if (fc_ev_mdl=="def"){
    if (nrow(subset(fc_ev_hist_dt,subset = Size == size & Technology == tmp_techno & `Fuel type`==fuel_type & Range==range & Source=="EPA"))!=0){
      fc_ev <- as.numeric(subset(fc_ev_hist_dt,subset = Size == size & Technology == tmp_techno & `Fuel type`==fuel_type & Range==range & Source=="EPA" & Year == 2015 & Data == "Combined",value))
    } else {
      fc_ev <- as.numeric(subset(fc_ev_hist_dt,subset = Size == size_GREET & Technology == tmp_techno & `Fuel type`==fuel_type & Range==range & Source=="GREET",value))
    }
  } else {
    if (fc_ev_mdl=="low"){
      fc_ev_fun<-min
    } else if (fc_ev_mdl=="high"){
      fc_ev_fun<-max}
    fc_ev <- fc_ev_fun(subset(fc_ev_hist_dt,subset = Size%in% c(size,size_GREET) & Technology == tmp_techno & `Fuel type`==fuel_type & Range==range,value))
  }
  return(fc_ev)
}
