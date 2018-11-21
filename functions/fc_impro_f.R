fc_impro_f<-function(size,
                     techno,
                     year,
                     fuel_type, 
                     fc_impro=NA,
                     aeo_scen=NA,
                     fast_mode="n"){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("fc_impro_f")
  #Called functions
  fleet_hist_fc_f_res <- do.call(fun_res_f,list(fun_name="fleet_fc_hist_f",fast_mode=fast_mode))
  fleet_fc_hist <- fleet_hist_fc_f_res[["fleet_fc_hist"]]
  hist_fc<-subset(fleet_fc_hist,Technology==techno&Size==size&`Fuel type`==fuel_type)
  #Inputs
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  improvt_dt<-read.csv("inputs/fleet_fc_impro_aeo.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other paramaters
  first_yr <- max(hist_fc$Year)
  #i_fc_value is the value of FC in year-1 without anything else than technological improvement
  i_fc_value <- subset(hist_fc,Year==first_yr)[,"Value"]
  if (fc_impro=="n"){
    imp <- 0
    interm_fc_value <- 0
  }else {
    if (!grepl("_",aeo_scen)){
      temp_aeo_scen <- aeo_scen
    } else {
      temp_aeo_scen <- substring(aeo_scen,1,as.numeric(regexpr(pattern="_",aeo_scen))-1)
    }
    #Interm_fc_value is theoritical fc value at year year-1 (initial value times all the relative annual changes)
    interm_fc_value <- i_fc_value*prod(t(subset(improvt_dt,Size==size & Technology==techno & Aeo_case==temp_aeo_scen & Fc_impro==fc_impro)[,as.character(first_yr:(year-1))]/100+1))
    #imp is in % of i_fc_value of FC for year
    imp <- subset(improvt_dt,Size==size & Technology==techno & Aeo_case==temp_aeo_scen & Fc_impro==fc_impro)[,as.character(year)]/100
  }
  #abs_impro_value is the absolute value of fuel improvement (not relative)
  abs_impro_value <- interm_fc_value*imp
  return(abs_impro_value) 
}
