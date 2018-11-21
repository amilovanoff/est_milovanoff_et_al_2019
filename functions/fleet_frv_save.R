#Function: Fuel Reduction Values used in the model
fleet_frv_f<-function(frv_impro=NA,
                      frv_pwt_r=NA,
                      fast_mode="n"){
  #Attribute arguments
  source("architecture/attribute_f.R",local=TRUE)
  #Source
  source("functions/frv_kim_f.R",local = TRUE)
  #Input files
  vehicle_technology <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other functions
  fleet_mc_proj_f_res <- do.call(fun_res_f,list(fun_name="fleet_mc_proj_f",fast_mode=fast_mode))
  fleet_fc_proj_f_res <- do.call(fun_res_f,list(fun_name="fleet_fc_proj_f",fast_mode=fast_mode))
  fleet_wgt_dt <- fleet_mc_proj_f_res[["fleet_wgt_dt"]]
  fleet_fc_dt <- fleet_fc_proj_f_res[["fleet_FC_dt"]]
  #Create output file
  dt_col<-c("Size","Technology","Fuel type","Unit","FRV")
  frv_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  y=2015
  for (size in c("Car","Light truck")){
    for (techno in unique(vehicle_technology$own)){
      i_wgt<-as.numeric(subset(fleet_wgt_dt,Scenario=="BAU" & Technology==techno & Size==size,select=as.character(y)))
      #Fuel types per technology
      fuel_l <-unlist(strsplit(vehicle_technology$`Fuel type`[which(vehicle_technology$own == techno)][1], ";")) 
      for (fuel_type in fuel_l) {
        i_FC <- subset(fleet_fc_dt,Scenario=="BAU" & Technology==techno & Size==size & `Fuel type`==fuel_type & Year==y)[,"Value"]
        unit <- paste0(as.character(subset(fleet_fc_dt,Scenario=="BAU" & Technology==techno & Size==size & `Fuel type`==fuel_type & Year==y,select=Unit)),"100kg")
        frv <- do.call(frv_kim_f,list(year=y,size=size,techno=techno,fuel_type=fuel_type,i_wgt=i_wgt,i_FC=i_FC))
        frv_dt[nrow(frv_dt)+1,c("Size","Technology","Fuel type","Unit","FRV")]<-c(size,techno,fuel_type,unit,frv)
        }
    }
  } 
  return(frv_dt)
}
if (exists("frv_dt")){rm(list="frv_dt")}
for (frv_pwt_r in c(0,0.5,1)){
  fleet_frv_f_res <- do.call(fleet_frv_f,list(frv_impro="Med",frv_pwt_r=frv_pwt_r,fast_mode="y"))
  fleet_frv_f_res[,"Powertrain resizing"]<-frv_pwt_r
  if (exists("frv_dt")){
    frv_dt<-rbind(frv_dt,fleet_frv_f_res)
  } else {
    frv_dt<-fleet_frv_f_res
  }
}
write.csv(frv_dt,"inputs/fleet_frv_stat.csv",row.names = FALSE)
