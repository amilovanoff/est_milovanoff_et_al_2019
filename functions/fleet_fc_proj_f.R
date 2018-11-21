###>Function: Calculates the fuel consumption by scenario from 2016 onwards
fleet_fc_proj_f<-function(last_yr = NA,
                          fc_impro=NA,
                          fast_mode="n"){
  #Source
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f(fun_name = "fleet_fc_proj_f")
  #Called functions
  source("functions/fc_upgrade_f.R",local = TRUE)
  #Input files
  vehicle_technology <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  fuel_conv<-read.csv("inputs/fuel_conversion.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Function's outputs
  fleet_hist_fc_f_res<-do.call(fun_res_f,list(fun_name="fleet_fc_hist_f",fast_mode=fast_mode))
  fleet_hist_fc<-fleet_hist_fc_f_res[["fleet_fc_hist"]]
  fleet_mc_proj_f_res<-do.call(fun_res_f,list(fun_name="fleet_mc_proj_f",fast_mode=fast_mode))
  fleet_wgt_dt<-fleet_mc_proj_f_res[["fleet_wgt_dt"]]
  fleet_wgt_svg_dt<-fleet_mc_proj_f_res[["fleet_wgt_svg_dt"]]
  #Other parameters
  first_yr<-max(fleet_hist_fc$Year)+1
  LWscen_l<-unique(fleet_wgt_svg_dt$Scenario)
  if (exists("fleet_FC_dt",inherits = FALSE)){remove(fleet_FC_dt)}
  #Output files
  dt_col<-c("Data","Unit","LWscen","Size","Technology","Fuel type","Year","value")
  frv_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  fc_impro_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (LWscen in LWscen_l){
    for (techno in unique(vehicle_technology$own)){
      for (size in c("Car","Light truck")){
        wgt_dt <- subset(fleet_wgt_dt,Size==size & Technology==techno & Scenario==LWscen)
        wgt_svg_dt <- subset(fleet_wgt_svg_dt,Size==size & Technology==techno & Scenario==LWscen)
        fuel_l <- unlist(strsplit(vehicle_technology$`Fuel type`[which(vehicle_technology$own == techno)][1], ";"))
        for (fuel_type in fuel_l) {
          FC_dt <- subset(fleet_hist_fc,Technology==techno & Size==size & `Fuel type`==fuel_type)
          unit <- unique(FC_dt$Unit)
          for (y in first_yr:last_yr){
            #wgt_svg is the weight savings between y and y-1 in kg. Negative if weight savings.
            wgt_svg <- wgt_svg_dt[1,as.character(y)]
            #wgt is the weight of the vehicle in y.
            i_wgt <- wgt_dt[1,as.character(y)]
            #i_FC is the fuel consumption at year t-1
            i_FC <- subset(FC_dt,Technology==techno & Size==size & `Fuel type`==fuel_type & Year==(y - 1))[,"Value"]
            #Call function to calculate FC at year y
            fc_upgrade_f_res <- do.call(fc_upgrade_f,list(year=y,size=size,techno=techno,fuel_type=fuel_type,i_FC=i_FC,i_wgt=i_wgt,wgt_svg=wgt_svg))
            new_fc <- fc_upgrade_f_res[[1]]
            FRV <- fc_upgrade_f_res[[2]]
            delta_fc <- fc_upgrade_f_res[[3]]
            #Fill FC_dt
            FC_dt[nrow(FC_dt)+1,] <- FC_dt[nrow(FC_dt),]
            FC_dt[nrow(FC_dt),c("Year","Value")] <- c(y,new_fc)
            #Fill frv_dt
            frv_dt[nrow(frv_dt)+1,] <- c("FRV",paste0(unit,"100kg"),LWscen,size,techno,fuel_type,y,FRV)
            #Fill fc_impro_dt
            fc_impro_dt[nrow(fc_impro_dt)+1,] <- c("Absolute fuel consumption improvement", unit,LWscen,size,techno,fuel_type,y,delta_fc)
          }
          FC_dt[,"Scenario"]<-LWscen
          if (exists("fleet_FC_dt",inherits = FALSE)){
            fleet_FC_dt<-rbind(fleet_FC_dt,FC_dt)
          } else {
            fleet_FC_dt<-FC_dt
          }
        }
      }
    }
  }
  #Format
  dt_col <- c("Year","value")
  frv_dt[,dt_col] <- sapply(dt_col,function(x) as.numeric(frv_dt[,x]))
  fc_impro_dt[,dt_col] <- sapply(dt_col,function(x) as.numeric(fc_impro_dt[,x]))
  
  results<-list(fleet_FC_dt=fleet_FC_dt,
                fleet_frv_dt=frv_dt,
                fleet_fc_impro_dt=fc_impro_dt)
  return(results)
}
